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

import           Control.Arrow             (second)
import           Control.Error
import           Control.Monad
import qualified Data.HashMap.Strict       as Map
import           Data.List                 (isPrefixOf)
import           Data.Monoid
import qualified Data.Text                 as Text
import qualified Khan.CLI.Ansible          as Ansible
import qualified Khan.CLI.Artifact         as Artifact
import qualified Khan.CLI.Cluster          as Cluster
import qualified Khan.CLI.DNS              as DNS
import qualified Khan.CLI.Group            as Group
import qualified Khan.CLI.Image            as Image
import qualified Khan.CLI.Launch           as Launch
import qualified Khan.CLI.Metadata         as Metadata
import qualified Khan.CLI.Profile          as Profile
import qualified Khan.CLI.Routing          as Routing
import qualified Khan.CLI.SSH              as SSH
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.EC2.Metadata  as Meta
import qualified Options.Applicative       as Opt
import           Options.Applicative.Types (ParserPrefs)
import           System.Environment
import           System.Exit               (exitWith)
import           System.IO                 (hPutStrLn, stderr)

main :: IO ()
main = runScript $ do
    (as, es) <- scriptIO $
        (,) <$> getArgs
            <*> getEnvironment

    when ("--bash-completion-index" `elem` as) . void . liftIO $
        uncurry customExecParser (parserInfo mempty)

    ec2 <- Meta.ec2
    mr  <- regionalise ec2
    cmd <- either failure return $ parseProgram as es mr
    fmapLT format $ run ec2 cmd
  where
    failure ParserFailure{..} = liftIO $ getProgName
        >>= errMessage
        >>= hPutStrLn stderr
        >>  exitWith errExitCode

    regionalise False = return Nothing
    regionalise True  = do
        t  <- Text.unpack <$> meta AvailabilityZone
        az <- tryRead ("Failed to read Availability Zone from: " ++ t) t
        return $! Just $ azRegion az

    format (Err msg) = msg
    format ex        = show ex

    run ec2 (c@Common{..}, Command f x) = do
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
             -> Either ParserFailure (Common, Command)
parseProgram as es mr = uncurry execParserPure (parserInfo envMap) merged
  where
    merged = mappend argSet
        . reverse
        . concatMap append
        $ map (second prefix)
            [ ("KHAN_REGION", regionLong)
            , ("KHAN_RKEYS",  rkeysLong)
            , ("KHAN_LKEYS",  lkeysLong)
            , ("KHAN_CACHE",  cacheLong)
            , ("KHAN_CONFIG", configLong)
            ]

    append (k, v)
        | v `elem` argSet = []
        | otherwise       = maybe [] (: [v]) $ k `Map.lookup` envMap

    argSet
        | Just r <- mr
        , region `notElem` as = region : show r : as
        | otherwise           = as

    envMap = Map.fromList [(k, v) | (k, v) <- es, "KHAN_" `isPrefixOf` k]

    region = prefix regionLong
    prefix = mappend "--"

parserInfo :: EnvMap -> (ParserPrefs, ParserInfo (Common, Command))
parserInfo env = (prefs showHelpOnError, Opt.info parser idm)
  where
    parser = (,)
        <$> commonParser
        <*> hsubparser
             ( Ansible.commands env
            <> Artifact.commands
            <> Cluster.commands env
            <> DNS.commands
            <> Group.commands env
            <> Image.commands
            <> Launch.commands env
            <> Metadata.commands
            <> Profile.commands env
            <> Routing.commands env
            <> SSH.commands env
             )
