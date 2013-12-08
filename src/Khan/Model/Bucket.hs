{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Error
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import qualified Khan.Model.Object         as Object
import           Khan.Prelude
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import qualified Shelly                    as Shell

-- FIXME: Add async download
download :: Text -> Maybe Text -> FilePath -> AWS Bool
download b p dir = do
    log "Listing bucket '{}' contents" [b]
    GetBucketResponse{..} <- send $
        GetBucket b (Delimiter '/') (prefix p) 100 Nothing
    -- FIXME: Need to paginate

    as <- mapM (async . retrieve) (filter valid gbrContents)
    bs <- mapM wait as

    return $ or bs
  where
    prefix (Just x) = Just . fromMaybe x $ Text.stripPrefix "/" x
    prefix Nothing  = Nothing

    valid Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = True

    retrieve Contents{..} = do
        let dest = dir </> Path.fromText bcKey
        shell . Shell.mkdir_p $ Path.parent dest
        Object.download b bcKey dest
