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
    , defaultEnv
    , defaultVersion
    , defaultUser
    , defaultCache
    ) where

import Data.Version
import Khan.Prelude

accessKey, secretKey :: String
accessKey = "ACCESS_KEY_ID"
secretKey = "SECRET_ACCESS_KEY"

defaultEnv :: Text
defaultEnv = "dev"

defaultVersion :: Version
defaultVersion = Version [0] []

defaultUser :: Text
defaultUser = "ubuntu"

defaultCache :: Int
defaultCache = 360
