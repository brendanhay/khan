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
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Format           as Format
import qualified Data.Text.Lazy.IO          as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.Instance        as Instance
import qualified Khan.Model.Key             as Key
import qualified Khan.Model.Tag             as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2            hiding (Failed, Image)
import qualified Shelly                     as Shell
import           System.Directory
import           System.IO                  (stdout)
import qualified System.Posix.Files         as Posix

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

instance Options Inventory

data Ansible = Ansible
    { aEnv    :: !Env
    , aKey    :: Maybe FilePath
    , aBin    :: Maybe Text
    , aRetain :: !Int
    , aForce  :: !Bool
    , aArgs   :: [String]
    }

ansibleParser :: EnvMap -> Parser Ansible
ansibleParser env = Ansible
    <$> envOption env
    <*> keyOption
    <*> optional (textOption "bin" (short 'b')
        "Ansible binary name to exec.")
    <*> readOption "retention" "SECONDS" (value 360)
        "Number of seconds to cache inventory results for."
    <*> switchOption "force" False
        "Force update of any previously cached results."
    <*> argsOption str (action "file")
        "Pass through arugments to ansible."

instance Options Ansible where
    validate Ansible{..} =
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
    list = Instance.findAll [] [Tag.filter Tag.env [_env iEnv]] >>=
        foldlM hosts Map.empty

    hosts m RunningInstancesItemType{..} = case riitDnsName of
        Nothing   -> return m
        Just fqdn -> do
            t@Tags{..} <- Tag.lookup $ Tag.flatten riitTagSet

            let n@Names{..} = names t
                host        = Host fqdn tagDomain n cRegion
                update k    = Map.insertWith (<>) k (Set.singleton host)

            return $! foldl' (flip update) m
                [roleName, envName, Text.pack $ show cRegion, "khan", tagDomain]

playbook :: Common -> Ansible -> AWS ()
playbook c@Common{..} a@Ansible{..} = ansible c $ a
    { aBin  = aBin `mplus` Just "ansible-playbook"
    , aArgs = aArgs ++
        [ "--extra-vars"
        , concat [ "khan_region="
                 , show cRegion
                 , " khan_region_abbrev="
                 , Text.unpack $ abbreviate cRegion
                 , " khan_env="
                 , Text.unpack envName
                 , " khan_key="
                 , Text.unpack keyName
                 ]
        ]
    }
  where
    Names{..} = names aEnv

ansible :: Common -> Ansible -> AWS ()
ansible c@Common{..} a@Ansible{..} = do
    k <- maybe (Key.path cRKeys a cLKeys) return aKey
    i <- inventoryPath cCache aEnv

    let bin    = Path.fromText $ fromMaybe "ansible" aBin
        script = Path.encodeString $ i <.> "sh"
        inv    = Path.encodeString i

    whenM ((|| aForce) <$> exceeds inv) $ do
        log "Limit of {}s exceeded for {}, refreshing..." [show aRetain, inv]
        inventory c $ Inventory aEnv True True Nothing

    debug "Writing inventory script to {}" [script]
    liftIO . LText.writeFile script $
        Format.format "#!/usr/bin/env bash\nset -e\nexec cat {}\n" [i]

    debug "Setting +rwx on {}" [script]
    liftIO $ Posix.setFileMode script Posix.ownerModes

    let xs  = args k script
        out = [Shell.OutHandle $ Shell.UseHandle stdout]

    log "{} {}" [Shell.toTextIgnore bin, Text.unwords xs]
    liftEitherT . sh $ Shell.runHandles bin xs out (\_ _ _ -> return ())
  where
    args k s = argv ++ foldr' add []
        [ ("-i", Text.pack s)
        , ("--private-key", Shell.toTextIgnore k)
        ]

    argv = map Text.pack aArgs

    add (k, v) xs =
        if k `elem` argv
            then xs
            else k : v : xs

    exceeds i = liftIO $ do
        p <- doesFileExist i
        if not p
            then return True
            else do
                s  <- Posix.getFileStatus i
                ts <- getPOSIXTime
                return $
                    ts - Posix.modificationTimeHiRes s > fromIntegral aRetain
