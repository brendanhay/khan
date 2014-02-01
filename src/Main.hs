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
import           Data.Text.Format         (Shown(..))
import qualified Khan.CLI.Ansible         as Ansible
import qualified Khan.CLI.Artifact        as Artifact
import qualified Khan.CLI.Cluster         as Cluster
import qualified Khan.CLI.DNS             as DNS
import qualified Khan.CLI.Group           as Group
import qualified Khan.CLI.HealthCheck     as HealthCheck
import qualified Khan.CLI.Image           as Image
import qualified Khan.CLI.Launch          as Launch
import qualified Khan.CLI.Metadata        as Metadata
import qualified Khan.CLI.Profile         as Profile
import qualified Khan.CLI.Routing         as Routing
import qualified Khan.CLI.SSH             as SSH
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata (ec2)
import           Options.Applicative
import           System.Environment

programParser :: [(String, String)] -> Parser (Common, Command)
programParser env = (,)
    <$> commonParser env
    <*> hsubparser
         ( Cluster.commands
        <> Launch.commands
        <> Profile.commands
        <> Group.commands
        <> DNS.commands
        <> Artifact.commands
        <> Routing.commands
        <> SSH.commands
        <> Ansible.commands
        <> Metadata.commands
        <> Image.commands
         )

main :: IO ()
main = do
    env <- getEnvironment
    cmd <- customExecParser (prefs showHelpOnError) (info (programParser env) idm)
    runScript . fmapLT fmt $ run cmd
  where
    fmt (Err msg) = msg
    fmt ex        = show ex

    run (c, Command f x) = do
        unless (cSilent c) enableLogging
        p <- ec2
        c'@Common{..} <- regionalise c p
        validate c'
        rs <- contextAWS c' $ do
            debug "Running in region {}..." [Shown cRegion]
            y <- discover p c' x
            liftEitherT $ validate y
            f c' y
        hoistEither rs

    regionalise c False = return c
    regionalise c True  = fmapLT Err $ do
        az <- Text.unpack <$> meta AvailabilityZone
        r  <- tryRead ("Failed to read region from: " ++ az) az
        return $! c { cRegion = r }
