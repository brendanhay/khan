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

-- import qualified Khan.CLI.Ephemeral  as Ephemeral

import qualified Khan.CLI.Ansible           as Ansible
import qualified Khan.CLI.DNS               as DNS
import qualified Khan.CLI.Group             as Group
import qualified Khan.CLI.Host              as Host
import qualified Khan.CLI.Persistent        as Persistent
import qualified Khan.CLI.Profile           as Profile
import qualified Khan.CLI.Routing           as Routing
import qualified Khan.CLI.SSH               as SSH

import           Control.Error
import           Control.Monad
import qualified Data.ByteString.Char8      as BS
import           Data.Text.Encoding
import           Data.Text.Format           (Shown(..))
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata
import           Options.Applicative
import           Options.Applicative.Arrows

versionParser :: Parser (a -> a)
versionParser = infoOption "0.0.0"
    (long "version" <> help "Print version information.")

programParser :: Parser (Common, Command)
programParser = runA $ proc () -> do
    opt <- asA commonParser -< ()
    cmd <- (asA . hsubparser)
         ( Routing.commands
        <> Ansible.commands
        <> SSH.commands
        <> Group.commands
        <> Profile.commands
        <> DNS.commands
        <> Host.commands
        <> Persistent.commands
         ) -< ()
    A versionParser >>> A helper -< (opt, cmd)

programInfo :: ParserInfo (Common, Command)
programInfo = info programParser idm

main :: IO ()
main = execParser programInfo >>= runScript . fmapLT fmt . run
  where
    fmt (Err msg) = msg
    fmt ex        = show ex

    run (a, Command f x) = do
        b@Common{..} <- isEC2 >>= regionalise a >>= initialise
        validate b
        r <- lift . runAWS (creds b) cDebug . within cRegion $ do
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

    creds Common{..}
        | Just r <- cProfile = FromRole $! encodeUtf8 r
        | otherwise = FromKeys (BS.pack cAccess) (BS.pack cSecret)
