{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Internal.Defaults
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Defaults
    (
    -- * Environment
      accessKey
    , secretKey

    -- * Defaults
    , defaultCfgPath
    , defaultKeyDir
    , defaultEnv
    , defaultVersion
    , defaultUser
    ) where

import Control.Applicative
import Data.Version
import Filesystem.Path.CurrentOS
import Khan.Prelude
import System.Directory
import System.Environment

accessKey, secretKey :: String
accessKey = "ACCESS_KEY_ID"
secretKey = "SECRET_ACCESS_KEY"

defaultCfgPath :: IO FilePath
defaultCfgPath = do
    p <- doesDirectoryExist path
    decodeString <$>
        if p
            then return path
            else (++ "/config") <$> getCurrentDirectory
  where
    path = "/etc/khan"

defaultKeyDir :: String
defaultKeyDir = "keys"

defaultEnv :: Text
defaultEnv = "dev"

defaultVersion :: Version
defaultVersion = Version [0] []

defaultUser :: Text
defaultUser = "ubuntu"
