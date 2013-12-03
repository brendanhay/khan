{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.S3
import qualified Shell                     as Shell
import           System.Directory
import           System.IO.Streams         (InputStream)

download :: Text -> Text -> FilePath -> AWS Bool
download b k f = do
    p <- shell $ Shell.test_e f
    if p
        then return False
        else send_ (GetObject b k) >> return True

upload :: Text -> Text -> [AnyHeader] -> InputStream ByteString -> AWS Bool
upload b k hs body = do
    p <- isRight <$> sendCatch (HeadObject b k [])
    if p
        then return False
        else send_ (PutObject b k hs $ Streaming body) >> return True
