{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.Object
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Object
    ( download
    , upload
    , latest
    ) where

import           Control.Arrow
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.List         as Conduit
import           Data.SemVer
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           System.Directory

download :: Text -> Text -> FilePath -> AWS Bool
download b k (Path.encodeString -> f) = do
    p <- liftIO $ doesFileExist f
    if p
        then log "File '{}' already exists." [f]
        else do
            log "Downloading {}/{} to {}" [b, k, Text.pack f]
            rs <- send $ GetObject b (safeKey k) []
            responseBody rs $$+- Conduit.sinkFile f
    return $ not p

upload :: Text -> Text -> FilePath -> AWS Bool
upload b k (Path.encodeString -> f) = do
    p <- fmap isRight . sendCatch $ HeadObject b (safeKey k) []
    if p
        then log "Object '{}/{}' already exists." [b, k]
        else do
            log "Uploading {} to {}/{}" [Text.pack f, b, k]
            mb  <- requestBodyFile f
            bdy <- noteAWS "Unable to get file size: {}" [f] mb
            send_ $ PutObject b (safeKey k) [] bdy
    return $ not p

latest :: Text -> Text -> FilePath -> AWS Bool
latest b p f = do
    log "Paginating bucket '{}' contents" [b]
    mk <- paginate start
        $= Conduit.concatMap con
        $$ Conduit.fold max Nothing
    maybe (throwAWS "No semantically versioned keys in bucket '{}'" [b])
          (\(k, _) -> download b k f)
          mk
  where
    start = GetBucket b (Delimiter '/') (Text.stripPrefix "/" p) 250 Nothing

    con = map (Just . second ver . join (,) . bcKey) . gbrContents
    ver = hush . parseVersion
