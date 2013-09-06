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
    , Command  (..)
    , SubCommand

    -- * Program Helpers
    , Discover (..)
    , Validate (..)
    , defineOptions
    , check
    , runProgram
    , subCommand
    ) where

import Control.Error
import Control.Monad
import Data.List                 (find)
import Data.Text.Encoding
import Khan.Internal.Log
import Khan.Internal.OptionTypes
import Khan.Internal.Types
import Network.AWS
import Options                   hiding (boolOption)
import System.Environment
import System.Exit

defineOptions "Khan" $ do
    boolOption "kDebug" "debug" False
        "Log debug output"

    maybeTextOption "kRole" "iam-role" ""
        "IAM role - if specified takes precendence over access/secret keys."

    boolOption "kDiscovery" "discovery" False
        "Populate options from EC2 metadata. Requires --iam-role."

deriving instance Show Khan

instance Validate Khan where
    validate Khan{..} =
        check (kDiscovery && isNothing kRole)
            "--iam-role must be specified in order to use --discovery."

check :: (Monad m, Invalid a) => a -> String -> EitherT Error m ()
check x = when (invalid x) . throwT . Error

type SubCommand = Subcommand Khan (EitherT Error IO ())

subCommand :: (Show a, Options a, Discover a, Validate a)
           => String
           -> (a -> AWSContext ())
           -> SubCommand
subCommand name action = Options.subcommand name run
  where
    run k@Khan{..} o _ = do
        setLogging kDebug
        logStep "Running Khan..." k
        validate k
        auth <- credentials $ creds kRole
        runAWS auth kDebug $ do
            opts <- disco kDiscovery o
            validate opts
            action opts

    disco True  = (logDebug "Performing discovery..." >>) . discover
    disco False = (logDebug "Skipping discovery..."   >>) . return

    creds = maybe (FromEnv "ACCESS_KEY_ID" "SECRET_ACCESS_KEY")
                  (FromRole . encodeUtf8)

data Command = Command
    { cmdName :: String
    , cmdDesc :: String
    , cmdSubs :: [SubCommand]
    }

runProgram :: [Command] -> IO ()
runProgram cmds = do
    args <- getArgs
    case args of
        []     -> help "Additional subcommand required."
        (a:as) -> maybe (help $ "Unknown subcommand \"" ++ a ++ "\".")
                        (run as . cmdSubs) $ find ((== a) . cmdName) cmds
  where
    run argv sub =
        let parsed = parseSubcommand sub argv
        in case parsedSubcommand parsed of
            Just cmd -> runScript $ fmapLT awsError cmd
            Nothing  -> case parsedError parsed of
                Just ex -> do
                    putStrLn $ parsedHelp parsed
                    putStrLn ex
                    exitFailure
                Nothing -> do
                    putStrLn $ parsedHelp parsed
                    exitSuccess
    help msg = do
        putStrLn $ parsedHelp (parseOptions [] :: ParsedOptions Khan)
        putStrLn $ unlines ("Subcommands:" : map desc cmds ++ [""])
        putStrLn msg
        exitFailure

    desc Command{..} =
        "  " ++ cmdName ++ replicate (28 - length cmdName) ' ' ++ cmdDesc

    awsError (Error s) = s
    awsError (Ex e)    = show e
