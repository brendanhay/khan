{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Keys
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Keys where

import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           System.Directory

cert :: FilePath
cert = "./cert"

createKey :: Text -> AWS ()
createKey key = sendCatch (CreateKeyPair key) >>= either exist write
  where
    exist e = do
        checkError
            (("InvalidKeyPair.Duplicate" ==) . ecCode . head . eerErrors)
            (Left e)
        logInfo "KeyPair {} exists, not updating." [key]

    write k = do
        reg <- currentRegion
        let path = concat [cert, "/", show reg, ".", Text.unpack key, ".pem"]
        liftIO $ do
            createDirectoryIfMissing True cert
            Text.writeFile path $ ckqKeyMaterial k
        logInfo "Wrote new KeyPair to {}" [path]

