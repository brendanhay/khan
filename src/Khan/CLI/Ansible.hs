{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- Module      : Khan.CLI.Ansible
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Ansible (commands) where

import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List                  (intercalate, nub)
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.IO               as Text
import qualified Data.Vector                as Vector
-- import qualified Data.Map                   as Map
import qualified Data.HashMap.Strict        as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Builder     as Build
import qualified Filesystem.Path.CurrentOS  as Path
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.EC2
import qualified Shelly                     as Shell
import           System.Directory
import qualified System.Posix.Process       as Process

defineOptions "Inventory" $ do
    textOption "iDomain" "domain" ""
        "DNS domain. (required)"

    textOption "iEnv" "env" defaultEnv
        "Environment."

    boolOption "iList" "list" True
        "List."

    maybeTextOption "iHost" "host" ""
        "Host."

    boolOption "iSilent" "silent" False
        "Don't output inventory results to stdout."

deriving instance Show Inventory

instance Discover Inventory

instance Validate Inventory where
    validate Inventory{..} = do
        check iEnv    "--env must be specified."
        check iDomain "--domain must be specified."

defineOptions "Ansible" $ do
    textOption "aDomain" "domain" ""
        "DNS domain. (required)"

    textOption "aEnv" "env" defaultEnv
        "Environment."

    pathOption "aKeys" "keys" ""
        "Directory for private keys."

    boolOption "aSilent" "no-silent" True
        "Output inventory results to stdout."

    stringsOption "aArgs" "pass-through" []
        "Pass through arguments to ansible by specifying: -- [options]"

deriving instance Show Ansible

instance Discover Ansible where
    discover args a@Ansible{..} = do
        ks <- defaultPath aKeys $ configFile defaultKeyDir
        log "Using Key Path {}" [ks]
        return $! a { aArgs = aArgs ++ args, aKeys = ks }

instance Validate Ansible where
    validate Ansible{..} = do
        check aEnv    "--env must be specified."
        check aDomain "--domain must be specified."
        check aKeys   "--keys must be specified."

commands :: [Command]
commands =
    [ command inventory "inventory" "Output ansible compatible inventory."
        "Stuff."
    , command ansible "ansible" "Ansible."
        "Stuff."
    -- , command playbook "playbook" "Ansible Playbook."
    --     "Stuff."
    ]

-- FIXME: remove all 'keys'/keytpath style path options and use paths to explicit
-- resources like the .pem file directly
-- or inventory path
inventory :: Inventory -> AWS ()
inventory Inventory{..} = do
    i <- maybe list (const $ return Map.empty) iHost
    f <- Path.encodeString <$> inventoryPath

    let x = map (\(k, vs) -> Map.fromList [("name" :: Text, Aeson.toJSON k), ("values", Aeson.toJSON vs)]) $ Map.toList i
        q = Aeson.toJSON $ Map.singleton ("sections" :: Text) x

    t <- render "inventory.mustache" q

    liftIO . LBS.putStrLn $ Aeson.encodePretty i
    liftIO $ LBS.writeFile f t

    --liftIO $ unless iSilent (LBS.putStrLn lbs) >> LBS.writeFile file lbs
  where
    list = EC2.findInstances [] filters >>= foldlM attrs Map.empty

    filters =
        [ Filter ("tag:" <> envTag)    [iEnv]
        , Filter ("tag:" <> domainTag) [iDomain]
        ]

    attrs m RunningInstancesItemType{..} = case riitDnsName of
        Nothing  -> return m
        Just dns -> do
            Tags{..} <- lookupTags $ tags riitTagSet
            let Names{..} = createNames tagRole tagEnv tagVersion
                upd m' k  = Map.insertWith (<>) k (Set.singleton dns) m'
            return $ foldl' upd m [roleName, appName, imageName]

    tags = map (\ResourceTagSetItemType{..} -> (rtsitKey, rtsitValue))

ansible :: Ansible -> AWS ()
ansible Ansible{..} = do
    d <- expandPath aKeys
    f <- Path.encodeString <$> EC2.keyPath keyName d

    assertAWS "Unable to find {}" [f] . liftIO $ doesFileExist f

    inv <- inventoryPath

    let args = nub $ aArgs ++ ["-i", Path.encodeString inv, "--private-key", f]

    log "Writing inventory to {}" [inv]
    inventory $ Inventory aDomain aEnv True Nothing True

    log "ansible {}" [intercalate " " args]
    liftIO $ Process.executeFile "ansible" True args Nothing
  where
    Names{..} = createNames "base" aEnv Nothing

inventoryPath :: AWS FilePath
inventoryPath = (</> defaultInventory) <$> cachePath
