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

module Khan.CLI.Ansible (commands) where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (mplus)
import           Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.SemVer
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Format           as Format
import qualified Data.Text.Lazy.IO          as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.Image           as Image
import qualified Khan.Model.Instance        as Instance
import qualified Khan.Model.Key             as Key
import qualified Khan.Model.Profile         as Profile
import qualified Khan.Model.SecurityGroup   as Security
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2            hiding (Failed, Image)
import qualified Shelly                     as Shell
import           System.Directory
import qualified System.Posix.Files         as Posix

data Ansible = Ansible
    { aEnv    :: !Text
    , aKey    :: Maybe FilePath
    , aBin    :: Maybe Text
    , aRetain :: !Int
    , aForce  :: !Bool
    , aArgs   :: [String]
    }

ansibleParser :: Parser Ansible
ansibleParser = Ansible
    <$> envOption
    <*> keyOption
    <*> optional (textOption "bin" (short 'b')
        "Ansible binary name to exec.")
    <*> readOption "retention" "SECONDS" (value defaultCache)
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

data Inventory = Inventory
    { iEnv    :: !Text
    , iSilent :: !Bool
    , iList   :: !Bool
    , iHost   :: Maybe Text
    }

inventoryParser :: Parser Inventory
inventoryParser = Inventory
    <$> envOption
    <*> switchOption "silent" False
        "Don't output inventory results to stdout."
    <*> switchOption "list" True
        "List."
    <*> optional (textOption "host" mempty
        "Host.")

instance Options Inventory

data Image = Image
    { aRole     :: !Text
    , aVersion  :: Maybe Version
    , aPlaybook :: !FilePath
    , aImage    :: !Text
    , aType     :: !InstanceType
    , aPreserve :: !Bool
    }

imageParser :: Parser Image
imageParser = Image
    <$> roleOption
    <*> optional versionOption
    <*> pathOption "playbook" (short 'p' <> value "")
        "Path to the playbook to run on the base instance."
    <*> textOption "base" (short 'b')
        "Id of the base image/ami."
    <*> readOption "type" "TYPE" (value M1_Small)
        "Instance's type."
    <*> switchOption "preserve" False
        "Don't terminate the base instance on error."

instance Options Image where
    discover _ _ a@Image{..} = return $! a
        { aPlaybook = defaultPath aPlaybook $ Path.fromText "image.yml"
        }

instance Naming Image where
    names Image{..} = ver
        { profileName = "ami-builder"
        , groupName   = sshGroup "ami"
        }
      where
        ver = maybe (unversioned aRole "ami") (versioned aRole "ami") aVersion

commands :: Mod CommandFields Command
commands = mconcat
    [ command "ansible" ansible ansibleParser
        "Ansible Command."
    , command "playbook" playbook ansibleParser
        "Ansible Playbook."
    , command "inventory" inventory inventoryParser
        "Output Ansible compatible inventory."
    , command "image" image imageParser
        "Create Image."
    ]

ansible :: Common -> Ansible -> AWS ()
ansible c@Common{..} a@Ansible{..} = do
    k <- maybe (Key.path cBucket a cCerts) return aKey
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

    let xs = args k script

    log "{} {}" [Shell.toTextIgnore bin, Text.unwords xs]
    liftEitherT . sh . Shell.silently $
        Shell.runHandles bin xs [Shell.OutHandle Shell.Inherit] (\_ _ _ -> return ())
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
                 , Text.unpack aEnv
                 , " khan_key="
                 , Text.unpack $ aEnv <> "-khan"
                 ]
        ]
    }

inventory :: Common -> Inventory -> AWS ()
inventory Common{..} Inventory{..} = do
    j <- Aeson.encodePretty . JS <$> maybe list (const $ return Map.empty) iHost
    i <- inventoryPath cCache iEnv

    debug "Writing inventory to {}" [i]
    liftIO $ LBS.writeFile (Path.encodeString i) (j <> "\n")

    debug_ "Writing inventory to stdout"
    unless iSilent . liftIO $ LBS.putStrLn j
  where
    list = Instance.findAll [] [Filter ("tag:" <> envTag) [iEnv]] >>=
        foldlM hosts Map.empty

    hosts m RunningInstancesItemType{..} = case riitDnsName of
        Nothing   -> return m
        Just fqdn -> do
            Tags{..} <- lookupTags $ map tag riitTagSet

            let n@Names{..} = createNames tagRole tagEnv tagVersion
                host     = Host fqdn tagDomain n cRegion
                update k = Map.insertWith (<>) k (Set.singleton host)

            return $! foldl' (flip update) m
                [roleName, envName, Text.pack $ show cRegion, "khan", tagDomain]

    tag ResourceTagSetItemType{..} = (rtsitKey, rtsitValue)

image :: Common -> Image -> AWS ()
image c@Common{..} d@Image{..} = do
    shell (Shell.which "ansible-playbook") >>=
        void . noteAWS "Command {} doesn't exist." ["ansible-playbook" :: Text]

    log "Checking if target Image {} exists..." [imageName]
    as <- Image.findAll [] [Filter "name" [imageName]]

    unless (null as) $
        throwAWS "Image {} already exists, exiting..." [imageName]

    log "Looking for base Images matching {}" [aImage]
    a <- async $ Image.find [aImage] []

    log "Looking for IAM Profile matching {}" [profileName]
    i <- async $ Profile.find d

    ami <- diritImageId <$> wait a
    log "Found base Image {}" [ami]

    wait_ i <* log "Found IAM Profile {}" [profileName]

    k <- async $ Key.create cBucket d cCerts
    g <- async $ Security.update d sshRules

    wait_ k <* log "Found KeyPair {}" [keyName]
    wait_ g <* log "Found Role Group {}" [groupName]

    az  <- shuffle "bc"
    mr  <- listToMaybe . map riitInstanceId <$>
        Instance.run d ami aType (AZ cRegion az) 1 1 False
    iid <- noteAWS "Failed to launch any Instances using Image {}" [ami] mr
    log "Launched Instance {}" [iid]

    Instance.wait [iid]

    e <- contextAWS c $ do
        log "Finding public DNS for {}" [iid]
        mdns <- join . listToMaybe . map riitDnsName <$>
            Instance.findAll [iid] []
        dns  <- noteAWS "Failed to retrieve DNS for {}" [iid] mdns

        let js = LBS.unpack . encode $ ImageInput n cRegion dns

        -- FIXME: poll for ssh connectivity
        -- ssh -q -o “BatchMode=yes” user@host “echo 2>&1″

        log "Waiting 20 seconds for SSH on {}" [iid]
        liftIO $ threadDelay $ 1000000 * 20

        log "Running Playbook {}" [aPlaybook]
        playbook c . Ansible "ami" Nothing Nothing 36000 False $
            [ "-i", Text.unpack $ dns <> ",localhost,"
            , "-e", js
            , Path.encodeString aPlaybook
            ]

        void $ Image.create iid imageName []

    if (isLeft e && aPreserve)
        then log "Error creating Image, preserving base Instance {}" [iid]
        else do
            log_ "Terminating base instance"
            send_ $ TerminateInstances [iid]

    const () <$> hoistError e
  where
    n@Names{..} = names d
