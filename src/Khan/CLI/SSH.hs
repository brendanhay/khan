{-# LANGUAGE BangPatterns      #-}
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
import           Data.List                 (intercalate)
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import qualified Khan.AWS.EC2              as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.EC2
import           System.IO                 hiding (FilePath)
import qualified System.Posix.Process      as Posix

data SSH = SSH
    { sRole :: !Text
    , sEnv  :: !Text
    , sUser :: !Text
    , sKey  :: !FilePath
    , sArgs :: [String]
    } deriving (Show)

sshParser :: Parser SSH
sshParser = SSH
    <$> roleOption
    <*> envOption
    <*> textOption "user" (value "ubuntu") "SSH User."
    <*> pathOption "key" (value "") "SSH Key."
    <*> argsOption str mempty "Pass through arugments to ssh."

instance Options SSH where
    discover s@SSH{..} = do
        f <- if invalid sKey then keyPath $ names s else return sKey
        return $! s { sKey = f }

    validate SSH{..} = do
        check sRole "--role must be specified."
        check sEnv  "--env must be specified."
        check sUser "--user must be specified."
        checkPath sKey " specified by --key must exist."

instance Naming SSH where
    names SSH{..} = unversioned sRole sEnv

commands :: Mod CommandFields Command
commands = command "ssh" ssh sshParser
    "Long description."
  where
    ssh _ SSH{..} = do
        dns <- mapMaybe riitDnsName <$> EC2.findInstances []
            [ Filter ("tag:" <> envTag)  [sEnv]
            , Filter ("tag:" <> roleTag) [sRole]
            ]

        let cs = map (first show) $ zip ([1..] :: [Int]) dns

        mapM_ (\(n, addr) -> log "{}) {}" [n, Text.unpack addr]) cs

        unless (null cs) $ do
            x <- liftIO $ do
                hSetBuffering stdout NoBuffering
                putStr "Select the host to connect to: "
                getLine

            addr <- noteAWS "Invalid host selection '{}'." [x] $ x `lookup` cs

            let args = [ "-i" ++ Path.encodeString sKey
                       , Text.unpack $ Text.concat [sUser, "@", addr]
                       ] ++ sArgs

            log "ssh {}" [intercalate " " args]
            liftIO $ Posix.executeFile "ssh" True args Nothing
