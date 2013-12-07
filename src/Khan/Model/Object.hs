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
    ) where

import           Control.Error
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           System.Directory

download :: Text -> Text -> FilePath -> AWS Bool
download b k (Path.encodeString -> f) = do
    p <- liftIO $ doesFileExist f
    if p
        then log "File '{}' already exists." [f]
        else do
            rs <- send $ GetObject b k []
            responseBody rs $$+- Conduit.sinkFile f
    return $ not p

upload :: Text -> Text -> FilePath -> AWS Bool
upload b k (Path.encodeString -> f) = do
    p <- fmap (statusIsSuccessful . responseStatus) . send $ HeadObject b k []
    if p
        then log "Object '{}{}' already exists." [b, k]
        else do
            mb  <- requestBodyFile f
            bdy <- noteAWS "Unable to get file size: {}" [f] mb
            send_ $ PutObject b k [] bdy
    return $ not p
