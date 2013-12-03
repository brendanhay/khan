{-# LANGUAGE Arrows            #-}
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

import qualified Khan.CLI.Ansible           as Ansible
import qualified Khan.CLI.Artifact          as Artifact
import qualified Khan.CLI.Check             as Check
import qualified Khan.CLI.DNS               as DNS
import qualified Khan.CLI.Ephemeral         as Ephemeral
import qualified Khan.CLI.Group             as Group
import qualified Khan.CLI.Host              as Host
import qualified Khan.CLI.Persistent        as Persistent
import qualified Khan.CLI.Profile           as Profile
import qualified Khan.CLI.Routing           as Routing
import qualified Khan.CLI.SSH               as SSH

import           Control.Error
import           Control.Monad
import qualified Data.ByteString.Char8      as BS
import           Data.Text.Format           (Shown(..))
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata
import           Options.Applicative
import           Options.Applicative.Arrows

programParser :: Parser (Common, Command)
programParser = runA $ proc () -> do
    cmd <- (asA . hsubparser)
         ( Ansible.commands
        <> Artifact.commands
        <> Check.commands
        <> DNS.commands
        <> Ephemeral.commands
        <> Group.commands
        <> Host.commands
        <> Persistent.commands
        <> Profile.commands
        <> Routing.commands
        <> SSH.commands
         ) -< ()
    opt <- asA commonParser -< ()
    A helper -< (opt, cmd)

programInfo :: ParserInfo (Common, Command)
programInfo = info programParser idm

main :: IO ()
main = customExecParser (prefs showHelpOnError) programInfo >>=
    runScript . fmapLT fmt . run
  where
    fmt (Err msg) = msg
    fmt ex        = show ex

    run (a, Command f x) = do
        unless (cSilent a) enableLogging
        b@Common{..} <- isEC2 >>= regionalise a >>= initialise
        validate b
        r <- context b $ do
            debug "Running in region {}..." [Shown cRegion]
            p <- isEC2
            y <- discover p x
            liftEitherT $ validate y
            f b y
        hoistEither r

    regionalise o False = return o
    regionalise o True  = do
        az  <- BS.unpack . BS.init <$> metadata AvailabilityZone
        reg <- fmapLT Err $
            tryRead ("Failed to read region from: " ++ az) az
        return $! o { cRegion = reg }
