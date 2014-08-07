{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
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

import           Control.Arrow  ()
import           Data.List                    (isPrefixOf)
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Internal
import qualified Khan.Model.EC2.Instance      as Instance
import qualified Khan.Model.Key               as Key
import           Khan.Model.SSH               (Mode(..))
import qualified Khan.Model.SSH               as SSH
import qualified Khan.Model.Tag               as Tag
import           Khan.Prelude
import           Network.AWS.EC2
import           System.Environment
import           System.IO                    hiding (FilePath)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), group)

data SSH = SSH
    { sshRKeys :: !RKeysBucket
    , sshRole  :: !Role
    , sshEnv   :: !Env
    , sshKey   :: Maybe FilePath
    , sshUser  :: !Text
    , sshArgs  :: [String]
    } deriving (Show)

sshParser :: EnvMap -> Parser SSH
sshParser env = SSH
    <$> rKeysOption env
    <*> roleOption
    <*> envOption env
    <*> keyOption
    <*> userOption
    <*> argsOption str mempty "Pass through arugments to ssh."

instance Options SSH where
    validate SSH{..} =
        check sshEnv "--env must be specified."

instance Naming SSH where
    names SSH{..} = unversioned sshRole sshEnv

data SCP = SCP
    { scpRKeys  :: !RKeysBucket
    , scpRole   :: !Role
    , scpEnv    :: !Env
    , scpKey    :: Maybe FilePath
    , scpUser   :: !Text
    , scpMode   :: !Mode
    , scpArgs   :: [String]
    } deriving (Show)

scpParser :: EnvMap -> (FilePath -> FilePath -> Mode) -> Parser SCP
scpParser env mode = SCP
    <$> rKeysOption env
    <*> roleOption
    <*> envOption env
    <*> keyOption
    <*> userOption
    <*> modeParser
    <*> argsOption str mempty "Pass through arugments to scp."
  where
    modeParser = mode
        <$> pathOption "source" (short 's' <> action "file")
            "Source path."
        <*> pathOption "destination" (short 'd' <> action "file")
            "Destination path."

instance Options SCP where
    validate SCP{..} =
        check scpEnv "--env must be specified."

instance Naming SCP where
    names SCP{..} = unversioned scpRole scpEnv

commands :: EnvMap -> Mod CommandFields Command
commands env = mconcat
    [ command "ssh" ssh (sshParser env)
        "Display a multiple choice list of matching hosts to SSH into."
    , group "scp" "Manage Artifacts over SCP." $ mconcat
        [ command "upload" scp (scpParser env Upload)
            "Upload."
        , command "download" scp (scpParser env Download)
            "Download."
        ]
    ]

ssh :: Common -> SSH -> AWS ()
ssh Common{..} s@SSH{..} = do
    key <- maybe (Key.path sshRKeys s cLKeys) return sshKey
    info cVPN sshEnv sshRole >>= go key
  where
    go _ []  = log_ "No hosts found."
    go k [x] = exec k x
    go k xs = do
        mapM_ (pPrint . pretty) xs
        c <- choose
        x <- noteAWS "Invalid host selection '{}'." [c] $
            find ((c ==) . show . ordinal) xs
        exec k x

    exec k x = SSH.execSSH (address x) sshUser k sshArgs

    choose = liftIO $ do
        hSetBuffering stdout NoBuffering
        putStr "Select the host to connect to: "
        getLine

scp :: Common -> SCP -> AWS ()
scp Common{..} s@SCP{..} = do
    verify
    key <- maybe (Key.path scpRKeys s cLKeys) return scpKey
    info cVPN scpEnv scpRole >>= go key
  where
    go _ [] = log_ "No hosts found."
    go k xs = mapM (async . exec k) xs >>= mapM_ wait_

    exec k x = SSH.execSCP scpMode (address x) scpUser k scpArgs

    -- FIXME: ghetto check to ensure bash expansion hasn't occured accidently
    -- for a remote home directory
    verify = do
        h <- fromMaybe "" <$> liftIO (lookupEnv "HOME")
        when (h `isPrefixOf` path scpMode) $
           throwAWS "Unexpected bash expansion of $HOME: {}" [show scpMode]

    path (Upload   _ p) = Path.encodeString p
    path (Download p _) = Path.encodeString p

data Info = Info !Int !(Text, Text) !RunningInstancesItemType

ordinal :: Info -> Int
ordinal (Info n _ _) = n

address :: Info -> Text
address (Info _ a _) = snd a

info :: Bool -> Env -> Role -> AWS [Info]
info vpn env role = mapMaybe f . zip [1..] <$> Instance.findAll []
    [ Tag.filter Tag.env  [_env env]
    , Tag.filter Tag.role [_role role]
    ]
  where
    f (n, x) = Info n
        <$> Instance.address vpn x
        <*> pure x

instance Pretty Info where
    pretty (Info n (v, a) RunningInstancesItemType{..}) = " " <> int n <> ")"
        <+> addr
        <+> pretty riitInstanceId
        <+> pretty riitImageId
        <+> pretty (istName riitInstanceState)
      where
        addr | v /= a    = wrap a <+> pretty (stripText ".amazonaws.com" v)
             | otherwise = wrap v

        wrap x = "[" <> pretty x <> "]"
