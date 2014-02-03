{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
import qualified Data.Text           as Text
import           Khan.Internal
import qualified Khan.Model.Instance as Instance
import qualified Khan.Model.Key      as Key
import qualified Khan.Model.SSH      as SSH
import qualified Khan.Model.Tag      as Tag
import           Khan.Prelude
import           Network.AWS.EC2
import           System.IO           hiding (FilePath)

-- FIXME: Add scp/sftp
data SSH = SSH
    { sRole :: !Role
    , sEnv  :: !Env
    , sKey  :: Maybe FilePath
    , sUser :: !Text
    , sArgs :: [String]
    } deriving (Show)

sshParser :: Parser SSH
sshParser = SSH
    <$> roleOption
    <*> envOption
    <*> keyOption
    <*> userOption
    <*> argsOption str mempty "Pass through arugments to ssh."

instance Options SSH

instance Naming SSH where
    names SSH{..} = unversioned sRole sEnv

commands :: Mod CommandFields Command
commands = command "ssh" ssh sshParser
    "Long description."

ssh :: Common -> SSH -> AWS ()
ssh Common{..} s@SSH{..} = do
    key <- maybe (Key.path cBucket s cCerts) return sKey
    dns <- mapMaybe riitDnsName <$> Instance.findAll []
        [ Tag.filter Tag.env  [_env  sEnv]
        , Tag.filter Tag.role [_role sRole]
        ]
    go key dns
  where
    go _   []  = log_ "No hosts found, exiting..."
    go key [x] = SSH.exec x sUser key sArgs
    go key dns = do
        mapM_ (\(n, addr) -> log "{}) {}" [n, Text.unpack addr]) cs
        x <- choose
        a <- noteAWS "Invalid host selection '{}'." [x] $ x `lookup` cs
        SSH.exec a sUser key sArgs
      where
        cs = map (first show) $ zip ([1..] :: [Int]) dns

    choose = liftIO $ do
        hSetBuffering stdout NoBuffering
        putStr "Select the host to connect to: "
        getLine

