{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.Bucket
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Bucket
    ( download
    ) where

import           Data.Conduit
import qualified Data.Conduit.List         as Conduit
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import qualified Khan.Model.Object         as Object
import           Khan.Prelude
import           Network.AWS.S3
import qualified Shelly                    as Shell

download :: Int -> Text -> Maybe Text -> FilePath -> AWS Bool
download n b p dir = do
    log "Paginating bucket '{}' contents" [b]
    or <$> (paginate start
        $= Conduit.concatMap (filter match . gbrContents)
        $= chunked []
        $= Conduit.mapM (mapM (async . retrieve))
        $= Conduit.concatMapM (mapM wait)
        $$ Conduit.consume)
  where
    start = GetBucket b (Delimiter '/') (prefix p) 250 Nothing

    prefix (Just x) = Just . fromMaybe x $ Text.stripPrefix "/" x
    prefix Nothing  = Nothing

    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = True

    retrieve Contents{..} = do
        let dest = dir </> Path.fromText bcKey
        shell . Shell.mkdir_p $ Path.parent dest
        Object.download b bcKey dest

    chunked xs = do
        mx <- await
        case mx of
            Just x | length xs < (n - 1) -> chunked (x : xs)
            Just x                       -> yield (x : xs) >> chunked []
            Nothing                      -> void $ yield xs
