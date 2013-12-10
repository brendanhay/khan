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
import qualified Data.Text                  as Text
import           Data.Text.Format           (Shown(..))
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata
import           Options.Applicative
import           Options.Applicative.Arrows
import           System.Environment

programParser :: Parser (Common, Command)
programParser = runA $ proc () -> do
    opt <- asA commonParser -< ()
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
    A helper -< (opt, cmd)

programInfo :: ParserInfo (Common, Command)
programInfo = info programParser idm

main :: IO ()
main = customExecParser (prefs showHelpOnError) programInfo >>=
    runScript . fmapLT fmt . run
  where
    fmt (Err msg) = msg
    fmt ex        = show ex

    run (c, Command f x) = do
        unless (cSilent c) enableLogging
        p <- isEC2
        c'@Common{..} <- setRegion c p >>= setBucket
        validate c'
        rs <- contextAWS c' $ do
            r <- getRegion
            debug "Running in region {}..." [Shown r]
            y <- discover p x
            liftEitherT $ validate y
            f c' y
        hoistEither rs

    setRegion c _     | isJust $ cRegion c = return c
    setRegion c False = do
        mr <- liftIO $ lookupEnv "KHAN_REGION"
        r  <- maybe (return NorthVirginia)
                    (tryRead "Failed to read valid region from KHAN_REGION")
                    mr
        return $! c { cRegion = Just r }
    setRegion c True  = do
        az <- BS.unpack . BS.init <$> metadata AvailabilityZone
        r  <- fmapLT Err $ tryRead ("Failed to read region from: " ++ az) az
        return $! c { cRegion = Just r }

    setBucket c
        | invalid $ cBucket c = do
            mb <- liftIO $ lookupEnv "KHAN_BUCKET"
            return $! c { cBucket = maybe (cBucket c) Text.pack mb }
        | otherwise = return c
