{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.Internal.Options
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Options
    (
    -- * Application Options
      Khan     (..)

    -- * Program Helpers
    , Discover (..)
    , Validate (..)
    , check
    , command
    ) where

import Control.Error
import Control.Monad
import Data.Text.Encoding
import Khan.Internal.Log
import Khan.Internal.OptionTypes
import Khan.Internal.Types
import Network.AWS

defineOptions "Khan" $ do
    boolOption "debug" "debug" False
        "Log debug output"

    maybeTextOption "role" "iam-role" ""
        "IAM role, if unspecified then credentials are retrieved from ENV."

    boolOption "discovery" "discovery" False
        "Populate options based on EC2 metadata. Requires --iam-role."

deriving instance Show Khan

instance Validate Khan where
    validate Khan{..} = do
        check (discovery && isNothing role)
             "--iam-role must be specified to use --discovery"

check :: (Monad m, Invalid a) => a -> String -> EitherT String m ()
check x = when (invalid x) . throwT

command :: (Options a, Discover a, Validate a)
        => String
        -> (a -> (EitherT AWSError AWS b -> EitherT AWSError IO b) -> Script b)
        -> Subcommand Khan (IO b)
command name action = subcommand name runner
  where
    runner k@Khan{..} o _ = runScript $ do
        setLogging debug
        logStep "Running Khan ..." k
        validate k
        opts <- disco discovery o
        validate opts
        action opts context
      where
        disco True  = (logDebug "Performing discovery..." >>) . discover
        disco False = (logDebug "Skipping discovery..." >>) . return

        context aws = credentials creds >>= \auth -> runAWS auth debug aws

        creds = maybe (FromEnv "ACCESS_KEY_ID" "SECRET_ACCESS_KEY")
                      (FromRole . encodeUtf8) role
