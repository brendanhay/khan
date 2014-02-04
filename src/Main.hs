{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
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

import           Control.Error
import           Control.Monad
import qualified Data.Text                as Text
import qualified Khan.CLI.Ansible         as Ansible
import qualified Khan.CLI.Artifact        as Artifact
import qualified Khan.CLI.Cluster         as Cluster
import qualified Khan.CLI.DNS             as DNS
import qualified Khan.CLI.Group           as Group
import qualified Khan.CLI.Image           as Image
import qualified Khan.CLI.Launch          as Launch
import qualified Khan.CLI.Metadata        as Metadata
import qualified Khan.CLI.Profile         as Profile
import qualified Khan.CLI.Routing         as Routing
import qualified Khan.CLI.SSH             as SSH
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.EC2.Metadata as Meta
import           Options.Applicative      (info)
import           System.Environment

main :: IO ()
main = runScript $ do
    (n, as, env) <- scriptIO $
        (,,) <$> getProgName
             <*> getArgs
             <*> getEnvironment

    ec2 <- Meta.ec2
    mr  <- regionalise ec2
    cmd <- either (\e -> scriptIO (errMessage e $ n) >>= left)
                  right
                  (execParser as env mr)

    fmapLT format $ run ec2 cmd
  where
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

execParser :: [String]
           -> [(String, String)]
           -> Maybe Region
           -> Either ParserFailure (Common, Command)
execParser as env mr =
    execParserPure (prefs showHelpOnError) (info parser idm) as
  where
    parser :: Parser (Common, Command)
    parser = (,)
        <$> commonParser env mr
        <*> hsubparser
             ( Ansible.commands
            <> Artifact.commands
            <> Cluster.commands
            <> DNS.commands
            <> Group.commands
            <> Image.commands
            <> Launch.commands
            <> Metadata.commands
            <> Profile.commands
            <> Routing.commands
            <> SSH.commands
             )
