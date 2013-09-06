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
import Data.Text.Encoding
import Khan.Internal.Log
import Khan.Internal.OptionTypes
import Khan.Internal.Types
import Network.AWS
import Options                hiding (boolOption)
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

type SubCommand a = Subcommand Khan (EitherT Error IO a)

subCommand :: (Show a, Options a, Discover a, Validate a)
           => String
           -> (a -> AWSContext b)
           -> SubCommand b
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

runProgram :: [(String, [SubCommand a])] -> IO a
runProgram cmds = do
    args <- getArgs
    case args of
        []       -> help
        (a:argv) -> maybe help (run argv) $ a `lookup` cmds
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
    help = do
        putStrLn $ parsedHelp (parseOptions [] :: ParsedOptions Khan)
        putStrLn $ unlines ("Subcommands:" : map (("  " ++) . fst) cmds ++ [""])
        putStrLn "No subcommand specified"
        exitFailure

    awsError (Error s) = s
    awsError (Ex e)    = show e
