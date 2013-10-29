{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import qualified Data.Text           as Text
import qualified Khan.CLI.Ansible    as Ansible
import qualified Khan.CLI.Ephemeral  as Ephemeral
import qualified Khan.CLI.Group      as Group
import qualified Khan.CLI.Host       as Host
import qualified Khan.CLI.Metadata   as Metadata
import qualified Khan.CLI.Persistent as Persistent
import qualified Khan.CLI.Profile    as Profile
import qualified Khan.CLI.Routing    as Routing
import           Khan.Internal
import           Khan.Prelude
import           Options             (subcommandInfo)
import           Options.Types

defineOptions "Complete" $ do
    maybeTextOption "iCmd" "command" ""
        "Command."

deriving instance Show Complete

instance Discover Complete
instance Validate Complete

complete :: Command
complete = command f "complete" "Bash command completion metadata." "Stuff."
  where
    f Complete{..} = case iCmd of
        Nothing -> mapM_ (liftIO . putStrLn . cmdName) commands
        Just c  -> do
            let subs  = map (subcommandInfo . cmdSub) commands
                cmd   = fromMaybe [] $ Text.unpack c `lookup` subs
                flags = concatMap (map ("--" ++) . optionInfoLongFlags) cmd
            mapM_ (liftIO . putStrLn) flags

commands :: [Command]
commands = concat
    [ Ephemeral.commands
    , Persistent.commands
    , Metadata.commands
    , Host.commands
    , Routing.commands
    , Profile.commands
    , Ansible.commands
    , Group.commands
    ]

main :: IO ()
main = runProgram $ complete : commands
