{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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
    , Discover (..)
    , Validate (..)
    , command

    -- * Option Constructors
    , recordTypeOption
    , failoverOption
    , regionOption
    , maybeTextOption
    , customOption
    ) where

import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Encoding
import Khan.Options.Internal
import Network.AWS
import Options

class Discover a where
    discover :: MonadIO m => a -> EitherT String m a

class Validate a where
    validate :: Monad m => a -> EitherT String m a

defineOptions "Khan" $ do
    boolOption "debug" "debug" False
        "Log debug output"

    maybeTextOption "role" "iam-role" ""
        "IAM role, if unspecified then credentials are retrieved from ENV."

    boolOption "discovery" "discovery" False
        "Attempt to populate options based on EC2 metadata. Requires --iam-role."

instance Validate Khan where
    validate k@Khan{..} = do
        when (discovery && isNothing role) $
            throwT "--iam-role must be specified to use --discovery"
        return k

command :: (Options a, Discover a, Validate a)
        => String
        -> (a -> Credentials -> EitherT String IO b)
        -> Subcommand Khan (IO b)
command name action = subcommand name $ \k o _ -> runScript $ do
    khan <- validate k
    opts <- validate =<< discover o
    action opts . maybe env (FromRole . encodeUtf8) $ role khan
  where
    env = FromEnv "ACCESS_KEY_ID" "SECRET_ACCESS_KEY"

