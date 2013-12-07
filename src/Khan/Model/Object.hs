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

upload :: Text -> Text -> FilePath -> AWS Bool
upload b k f = undefined
    -- p <- isRight <$> sendCatch (HeadObject b k [])
    -- if p
    --     then return False
    --     else do
    --         h <- liftIO $ openFile (Path.encodeString f) ReadMode
    --         i <- liftIO $ Streams.handleToInputStream h
    --         send_ (PutObject b k [] $ Streaming i) >> return True
