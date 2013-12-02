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

import           Control.Monad              (mplus)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Format           as Format
import qualified Data.Text.Lazy.IO          as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.Instance        as Instance
import           Khan.Prelude
import           Network.AWS.EC2            hiding (Failed)
import           System.Directory
import qualified System.Posix.Files         as Posix
import qualified System.Posix.Process       as Posix

data Ansible = Ansible
    { aEnv    :: !Text
    , aKey    :: !FilePath
    , aBin    :: Maybe Text
    , aRetain :: !Int
    , aCache  :: !FilePath
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
    <*> pathOption "cache" (value "" <> short 'c')
        "Path to the inventory file cache."
    <*> switchOption "force" False
        "Force update of any previously cached results."
    <*> argsOption str (action "file")
        "Pass through arugments to ansible."

instance Options Ansible where
    discover _ a@Ansible{..} = do
        f <- if invalid aKey then keyPath $ names a else return aKey
        c <- inventoryPath aCache aEnv
        return $! a { aKey = f, aCache = c }

    validate Ansible{..} = do
        checkPath aKey " specified by --key must exist."
        check aArgs "Pass ansible options through using the -- delimiter.\n\
                    \Usage: khan ansible [KHAN OPTIONS] -- [ANSIBLE OPTIONS]."

instance Naming Ansible where
    names Ansible{..} = unversioned "base" aEnv

data Inventory = Inventory
    { iEnv    :: !Text
    , iCache  :: !FilePath
    , iSilent :: !Bool
    , iList   :: !Bool
    , iHost   :: Maybe Text
    }

inventoryParser :: Parser Inventory
inventoryParser = Inventory
    <$> envOption
    <*> pathOption "cache" (value "" <> short 'c')
        "Path to the output inventory file cache."
    <*> switchOption "silent" False
        "Don't output inventory results to stdout."
    <*> switchOption "list" True
        "List."
    <*> optional (textOption "host" mempty
        "Host.")

instance Options Inventory where
    discover _ i@Inventory{..} = do
        c <- inventoryPath iCache iEnv
        return $! i { iCache = c }

commands :: Mod CommandFields Command
commands = mconcat
    [ command "ansible" ansible ansibleParser
        "Ansible."
    , command "playbook" playbook ansibleParser
        "Ansible Playbook."
    , command "inventory" inventory inventoryParser
        "Output ansible compatible inventory."
    ]

ansible :: Common -> Ansible -> AWS ()
ansible c Ansible{..} = do
    whenM ((|| aForce) <$> exceeds) $ do
        log "Limit of {}s exceeded for {}, refreshing..." [show aRetain, inv]
        inventory c $ Inventory aEnv aCache True True Nothing

    debug "Writing inventory script to {}" [script]
    liftIO . LText.writeFile script $
        Format.format "#!/usr/bin/env bash\nset -e\nexec cat {}\n" [inv]

    debug "Setting +rwx on {}" [script]
    liftIO $ Posix.setFileMode script Posix.ownerModes

    log "{} {}" [bin, unwords args]
    liftIO $ Posix.executeFile bin True args Nothing
  where
    args = aArgs ++ foldr' add []
        [ ("-i", script)
        , ("--private-key", Path.encodeString aKey)
        ]

    add (k, v) xs =
        if k `elem` aArgs
            then xs
            else k : v : xs

    exceeds = liftIO $ do
        p <- doesFileExist inv
        if not p
            then return True
            else do
                s  <- Posix.getFileStatus inv
                ts <- getPOSIXTime
                return $
                    ts - Posix.modificationTimeHiRes s > fromIntegral aRetain

    bin    = Text.unpack $ fromMaybe "ansible" aBin
    script = Path.encodeString $ aCache <.> "sh"
    inv    = Path.encodeString aCache

playbook :: Common -> Ansible -> AWS ()
playbook c a@Ansible{..} = do
    reg <- getRegion
    ansible c $ a
        { aBin  = mplus aBin (Just "ansible-playbook")
        , aArgs = aArgs ++
            [ "--extra-vars"
            , concat [ "khan_region="
                     , show reg
                     , " khan_region_abbrev="
                     , Text.unpack $ abbreviate reg
                     , " khan_env="
                     , Text.unpack aEnv
                     , " khan_key="
                     , Text.unpack $ aEnv <> "-khan"
                     -- , " khan_key_path="
                     -- ,
                     ]
            ]
        }

inventory :: Common -> Inventory -> AWS ()
inventory _ Inventory{..} = do
    j <- Aeson.encodePretty . JS <$> maybe list (const $ return Map.empty) iHost

    debug "Writing inventory to {}" [iCache]
    liftIO $ LBS.writeFile (Path.encodeString iCache) (j <> "\n")

    debug_ "Writing inventory to stdout"
    unless iSilent . liftIO $ LBS.putStrLn j
  where
    list = Instance.findAll [] [Filter ("tag:" <> envTag) [iEnv]] >>=
        foldlM hosts Map.empty

    hosts m RunningInstancesItemType{..} = case riitDnsName of
        Nothing   -> return m
        Just fqdn -> do
            reg      <- getRegion
            Tags{..} <- lookupTags $ map tag riitTagSet

            let n@Names{..} = createNames tagRole tagEnv tagVersion
                host     = Host fqdn tagDomain n reg
                update k = Map.insertWith (<>) k (Set.singleton host)

            return $! foldl' (flip update) m
                [roleName, envName, Text.pack $ show reg, "khan", tagDomain]

    tag ResourceTagSetItemType{..} = (rtsitKey, rtsitValue)
