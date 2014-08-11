{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Khan.CLI.Ansible
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Ansible
    ( commands

    -- * Convenience exports for the Image CLI
    , Ansible (..)
    , playbook
    ) where

import           Control.Monad              (mplus)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as Map
import qualified Data.HashSet               as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Format           as Format
import qualified Data.Text.Lazy             as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem                 as FS
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal
import           Khan.Model.Ansible
import qualified Khan.Model.EC2.Instance    as Instance
import qualified Khan.Model.Key             as Key
import qualified Khan.Model.Tag             as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2            hiding (Failed, Image)
import qualified System.Posix.Files         as Posix
import           System.Process             (callCommand)

data Inventory = Inventory
    { iEnv    :: !Env
    , iSilent :: !Bool
    , iList   :: !Bool
    , iHost   :: Maybe Text
    }

inventoryParser :: EnvMap -> Parser Inventory
inventoryParser env = Inventory
    <$> envOption env
    <*> switchOption "silent" False
        "Don't output inventory results to stdout."
    <*> switchOption "list" True
        "List."
    <*> optional (textOption "host" mempty
        "Host.")

instance Options Inventory where
    validate Inventory{..} =
        check iEnv "--env must be specified."

data Ansible = Ansible
    { aEnv    :: !Env
    , aRKeys  :: !RKeysBucket
    , aKey    :: Maybe FilePath
    , aBin    :: Maybe Text
    , aRetain :: !Int
    , aForce  :: !Bool
    , aArgs   :: [String]
    }

ansibleParser :: EnvMap -> Parser Ansible
ansibleParser env = Ansible
    <$> envOption env
    <*> rKeysOption env
    <*> keyOption
    <*> optional (textOption "bin" (short 'b')
        "Ansible binary name to exec.")
    <*> readOption "retention" "SECONDS" (value 360)
        "Number of seconds to cache inventory results for."
    <*> switchOption "force" False
        "Force update of any previously cached results."
    <*> argsOption str (action "file")
        "Pass through arguments to ansible."

instance Options Ansible where
    validate Ansible{..} = do
        check aEnv "--env must be specified."
        check aArgs "Pass ansible options through using the -- delimiter.\n\
                    \Usage: khan ansible [KHAN OPTIONS] -- [ANSIBLE OPTIONS]."

instance Naming Ansible where
    names Ansible{..} = unversioned "base" aEnv

commands :: EnvMap -> Mod CommandFields Command
commands env = mconcat
    [ command "ansible" ansible (ansibleParser env)
        "Run 'ansible' supplying it with khan_* facts and inventory."
    , command "playbook" playbook (ansibleParser env)
        "Run 'ansible-playbook' supplying it with khan_* facts and inventory."
    , command "inventory" inventory (inventoryParser env)
        "Output ansible compatible inventory in JSON format."
    ]

inventory :: Common -> Inventory -> AWS ()
inventory Common{..} Inventory{..} = do
    j <- Aeson.encodePretty . JS <$> maybe list (const $ return Map.empty) iHost
    i <- inventoryPath cCache iEnv

    debug "Writing inventory to {}" [i]
    liftIO $ LBS.writeFile (Path.encodeString i) (j <> "\n")

    debug_ "Writing inventory to stdout"
    unless iSilent . liftIO $ LBS.putStrLn j
  where
    list = localhost . foldl' hosts Map.empty <$> instances

    instances = Instance.findAll [] [Tag.filter Tag.env [_env iEnv]]
    localhost = Map.insert "localhost" (Set.singleton $ Localhost cRegion)

    hosts m i@RunningInstancesItemType{..} =
        case Instance.address cVPN i of
            Nothing            -> m
            Just (fqdn, addr)  -> fromMaybe m $ do
                t@Tags{..} <- hush (Tag.parse riitTagSet)

                let n@Names{..} = names t
                    host        = Host addr tagDomain n cRegion
                    update k    = Map.insertWith (<>) k (Set.singleton host)

                return $! foldl' (flip update) m
                    [ roleName
                    , envName
                    , regionToText cRegion
                    , "khan"
                    , tagDomain
                    , fqdn
                    ]

playbook :: Common -> Ansible -> AWS ()
playbook c a@Ansible{..} = ansible c $ a
    { aBin  = aBin `mplus` Just "ansible-playbook"
    , aArgs = overrides a aArgs
    }

ansible :: Common -> Ansible -> AWS ()
ansible c@Common{..} a@Ansible{..} = do
    liftEitherT (which bin)

    k <- maybe (Key.path aRKeys a cLKeys) return aKey
    i <- inventoryPath cCache aEnv

    let script = i <.> "sh"

    p <- (|| aForce) <$> exceeds i

    when p $ do
        log "Limit of {}s exceeded for {}, refreshing..." [P aRetain, B i]
        inventory c $ Inventory aEnv True True Nothing

    debug "Writing inventory script to {}" [B script]
    liftIO . FS.writeTextFile script . LText.toStrict $
        Format.format "#!/usr/bin/env bash\nset -e\nexec cat {}\n" [i]

    debug "Setting +rwx on {}" [B script]
    liftIO $ Posix.setFileMode (Path.encodeString script) Posix.ownerModes

    let cmd = unwords ["ANSIBLE_FORCE_COLOR=1", unwords (bin : args k script)]

    log "{}" [cmd]
    liftEitherT . sync $ callCommand cmd
  where
    bin = Text.unpack (fromMaybe "ansible" aBin)

    args k s = aArgs +$+
        [ ("-i",            Path.encodeString s)
        , ("--private-key", Path.encodeString k)
        ]

    exceeds i = liftIO $ do
        p <- FS.isFile i
        if not p
            then return True
            else do
                s  <- Posix.getFileStatus (Path.encodeString i)
                ts <- getPOSIXTime
                return $ ts - Posix.modificationTimeHiRes s > fromIntegral aRetain
