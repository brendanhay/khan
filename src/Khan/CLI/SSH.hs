{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.CLI.Group
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.SSH (commands) where

import           Control.Arrow
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import qualified Khan.Model.Instance       as Instance
import           Khan.Prelude
import           Network.AWS.EC2
import           System.IO                 hiding (FilePath)
import qualified System.Posix.Process      as Posix

-- FIXME: Add scp

data SSH = SSH
    { sRole :: !Text
    , sEnv  :: !Text
    , sKey  :: !FilePath
    , sUser :: !Text
    , sArgs :: [String]
    } deriving (Show)

sshParser :: Parser SSH
sshParser = SSH
    <$> roleOption
    <*> envOption
    <*> keyOption
    <*> textOption "user" (value "ubuntu" <> short 'u') "SSH User."
    <*> argsOption str mempty "Pass through arugments to ssh."

instance Options SSH where
    discover _ s@SSH{..} = do
        f <- if invalid sKey then keyPath $ names s else return sKey
        return $! s { sKey = f }

    validate SSH{..} =
        checkPath sKey " specified by --key must exist."

instance Naming SSH where
    names SSH{..} = unversioned sRole sEnv

commands :: Mod CommandFields Command
commands = command "ssh" ssh sshParser
    "Long description."

ssh :: Common -> SSH -> AWS ()
ssh _ SSH{..} = do
    dns <- mapMaybe riitDnsName <$> Instance.findAll []
        [ Filter ("tag:" <> envTag)  [sEnv]
        , Filter ("tag:" <> roleTag) [sRole]
        ]
    case dns of
        []  -> log_ "No hosts found, exiting..."
        [x] -> liftIO $ exec x
        _   -> do
            let cs = map (first show) $ zip ([1..] :: [Int]) dns
            mapM_ (\(n, addr) -> log "{}) {}" [n, Text.unpack addr]) cs
            x <- liftIO choose
            a <- noteAWS "Invalid host selection '{}'." [x] $ x `lookup` cs
            liftIO $ exec a
  where
    exec (args -> xs) = do
        log "ssh {}" [unwords xs]
        Posix.executeFile "ssh" True xs Nothing

    args addr =
        [ "-i" ++ Path.encodeString sKey
        , Text.unpack $ Text.concat [sUser, "@", addr]
        ] ++ sArgs

    choose = do
        hSetBuffering stdout NoBuffering
        putStr "Select the host to connect to: "
        getLine
