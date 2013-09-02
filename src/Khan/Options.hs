{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.Options
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Options
    (
    -- * Application Options
      Khan     (..)

    -- * Program Helpers
    , Validate (..)
    , command

    -- * Option Constructors
    , recordTypeOption
    , failoverOption
    , regionOption
    , maybeTextOption
    , customOption
    ) where

import Control.Applicative
import Control.Error
import Data.Text.Encoding
import Khan.Options.Internal
import Network.AWS
import Options

class Validate a where
    validate :: Monad m => a -> EitherT String m a

defineOptions "Khan" $ do
    boolOption "debug" "debug" False
        "Log debug output"

    maybeTextOption "role" "iam-role" ""
        "IAM role, if blank access/secret keys are retrieved from ENV."

instance Validate Khan where
    validate = return

command :: (Options a, Validate a)
        => String
        -> (a -> Auth -> EitherT String IO b)
        -> Subcommand Khan (IO b)
command name action = subcommand name $ \k o _ -> runScript $ do
    opts <- validate o
    cred <- maybe env (FromRole . encodeUtf8) . role <$> validate k
    auth <- hoistEither =<< credentials cred
    action opts auth
  where
    env = FromEnv "ACCESS_KEY_ID" "SECRET_ACCESS_KEY"

