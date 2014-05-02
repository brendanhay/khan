{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Control.Arrow            ((***))
import           Control.Error
import           Control.Monad
import qualified Data.HashMap.Strict      as Map
import           Data.List                (isPrefixOf)
import           Data.Monoid
import qualified Data.Text                as Text
import qualified Khan.CLI.Ansible         as Ansible
import qualified Khan.CLI.Artifact        as Artifact
import qualified Khan.CLI.Certificate     as Certificate
import qualified Khan.CLI.Cluster         as Cluster
import qualified Khan.CLI.DNS             as DNS
import qualified Khan.CLI.Group           as Group
import qualified Khan.CLI.Image           as Image
import qualified Khan.CLI.Launch          as Launch
import qualified Khan.CLI.Metadata        as Metadata
import qualified Khan.CLI.Profile         as Role
import qualified Khan.CLI.Routing         as Routing
import qualified Khan.CLI.SSH             as SSH
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.EC2.Metadata as Meta
import           Options.Applicative      (info)
import           System.Environment
import           System.Exit
import           System.IO                (BufferMode(..), hSetBuffering, hPutStrLn, stdout, stderr)

main :: IO ()
main = runScript $ do
    liftIO $ do
        hSetBuffering stdout LineBuffering
        hSetBuffering stderr LineBuffering

    (as, es, name) <- scriptIO $
        (,,) <$> getArgs
             <*> getEnvironment
             <*> getProgName

    when ("--bash-completion-index" `elem` as) . void . liftIO $
        uncurry customExecParser (parserInfo mempty)

    ec2 <- Meta.ec2
    mr  <- regionalise ec2

    case parseProgram as es mr of
        Success x           -> fmapLT format (run ec2 x)
        CompletionInvoked c -> complete c name
        Failure f           -> failure  f name
  where
    complete c n = liftIO $
        execCompletion c n >>= putStr >> exitSuccess

    failure f n =
        let (msg, c) = execFailure f n
         in liftIO $ do
                case c of
                    ExitSuccess -> putStrLn msg
                    _           -> hPutStrLn stderr msg
                exitWith c

    regionalise False = return Nothing
    regionalise True  = do
        t  <- Text.unpack <$> meta AvailabilityZone
        az <- tryRead ("Failed to read Availability Zone from: " ++ t) t
        return $! Just $ azRegion az

    format (Err msg) = msg
    format ex        = show ex

    run ec2 (c@Common{..}, Command f x) = do
        when cDebug . liftIO $ print c
        unless cSilent enableLogging
        validate c
        rs <- contextAWS c $ do
            debug "Running in region {}..." [show cRegion]
            y <- discover ec2 c x
            liftEitherT $ validate y
            f c y
        hoistEither rs

parseProgram :: [String]
             -> [(String, String)]
             -> Maybe Region
             -> ParserResult (Common, Command)
parseProgram as es mr = uncurry execParserPure (parserInfo upd) as
  where
    upd | Just r <- mr = Map.insert "KHAN_REGION" (Text.pack $ show r) env
        | otherwise    = env

    env = Map.fromList
        [join (***) Text.pack (k, v) | (k, v) <- es, "KHAN_" `isPrefixOf` k]

parserInfo :: EnvMap -> (ParserPrefs, ParserInfo (Common, Command))
parserInfo env =
    ( prefs $ showHelpOnError <> columns 100
    , info khanParser idm
    )
  where
    khanParser = (,)
        <$> commonParser env
        <*> hsubparser
             ( Ansible.commands env
            <> Artifact.commands
            <> Certificate.commands
            <> Cluster.commands env
            <> DNS.commands
            <> Group.commands env
            <> Image.commands env
            <> Launch.commands env
            <> Metadata.commands
            <> Role.commands env
            <> Routing.commands env
            <> SSH.commands env
             )
