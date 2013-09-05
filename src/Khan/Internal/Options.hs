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
    , awsCommand
    , subCommand
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
    validate Khan{..} =
        check (discovery && isNothing role)
            "--iam-role must be specified to use --discovery"

check :: (Monad m, Invalid a) => a -> String -> EitherT String m ()
check x = when (invalid x) . throwT

awsCommand :: (Show a, Options a, Discover a, Validate a)
           => String
           -> (a -> AWSContext b)
           -> Subcommand Khan (IO b)
awsCommand name action = subcommand name $ runner cmd
  where
    cmd Khan{..} opts creds = do
        auth <- fmapLT show $ credentials creds
        fmapLT show . runAWS auth debug $ action opts

subCommand :: (Show a, Options a, Discover a, Validate a)
           => String
           -> (a -> (AWSContext b -> Script b) -> Script b)
           -> Subcommand Khan (IO b)
subCommand name action = subcommand name $ runner cmd
  where
    cmd Khan{..} opts creds = action opts $ \aws ->
        fmapLT show $ credentials creds >>= \auth -> runAWS auth debug aws

runner :: (Show a, Validate a, Discover a)
       => (Khan -> a -> Credentials -> Script b)
       -> Khan
       -> a
       -> [String]
       -> IO b
runner cmd k@Khan{..} o _ = runScript $ do
    setLogging debug
    logStep "Running Khan ..." k
    validate k
    opts <- disco discovery o
    validate opts
    logStep "Running Subcommand ..." opts
    cmd k opts creds
  where
    disco True  = (logDebug "Performing discovery..." >>) . discover
    disco False = (logDebug "Skipping discovery..."   >>) . return

    creds = maybe (FromEnv "ACCESS_KEY_ID" "SECRET_ACCESS_KEY")
                  (FromRole . encodeUtf8) role
