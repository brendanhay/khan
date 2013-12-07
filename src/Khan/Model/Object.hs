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

import GHC.Int
import           Control.Error
import           Control.Exception
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.S3
import           Network.HTTP.Conduit
import qualified Shelly                    as Shell
import           System.Directory
import           System.IO                 hiding (FilePath)

download :: Text -> Text -> FilePath -> AWS Bool
download b k (Path.encodeString -> f) = do
    p <- liftIO $ doesFileExist f
    unless p $ do
        rs <- send $ GetObject b k []
        responseBody rs $$+- Conduit.sinkFile f
    return $ not p

-- FIXME: check if local file exists
upload :: Text -> Text -> FilePath -> AWS Bool
upload b k (Path.encodeString -> f) = do
    -- p <- isRight <$> sendCatch (HeadObject b k [])
    -- unless p $ do
    mn <- getFileSize
    n  <- noteAWS "Unable to get file size: {}" [f] mn
    send_ $ PutObject b k [] (requestBodySource n $ Conduit.sourceFile f)
    return True
  where
    getFileSize :: MonadIO m => m (Maybe Int64)
    getFileSize = liftIO $
        bracket (openBinaryFile f ReadMode)
                hClose
                (fmap (Just . fromIntegral) . hFileSize)
