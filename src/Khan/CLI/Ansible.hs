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
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (intercalate)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.EC2
import           System.Directory
import qualified System.Posix.Files         as Posix
import qualified System.Posix.Process       as Posix
import qualified System.Environment         as Env

defineOptions "Inventory" $ do
    textOption "iEnv" "env" defaultEnv
        "Environment."

    boolOption "iList" "list" True
        "List."

    maybeTextOption "iHost" "host" ""
        "Host."

    pathOption "iCache" "cache" ""
        "Path to the output inventory file cache."

    boolOption "iSilent" "silent" False
        "Don't output inventory results to stdout."

deriving instance Show Inventory

instance Discover Inventory where
    discover _ i@Inventory{..} = do
        c <- defaultPath iCache (cachePath "inventory")
        return $! i { iCache = c }

instance Validate Inventory where
    validate Inventory{..} =
        check iEnv "--env must be specified."

defineOptions "Ansible" $ do
    maybeTextOption "aBin" "bin" ""
        "Ansible binary name to exec."

    textOption "aEnv" "env" defaultEnv
        "Environment."

    pathOption "aKey" "key" ""
        "Path to the private key to use."

    boolOption "aSilent" "no-silent" True
        "Output inventory results to stdout."

    intOption "aRetain" "retention" defaultCache
        "Number of seconds to cache inventory results for."

    pathOption "aCache" "cache" ""
        "Path to the inventory file cache."

    boolOption "aForce" "force" False
        "Force update of any previously cached results."

    stringsOption "aArgs" "pass-through" []
        "Pass through arguments to ansible by specifying: -- [options]"

deriving instance Show Ansible

instance Discover Ansible where
    discover args a@Ansible{..} = do
        f <- if invalid aKey then keyPath $ names a else return aKey
        c <- defaultPath aCache (cachePath "inventory")
        return $! a { aKey = f, aCache = c, aArgs = aArgs ++ args }

instance Validate Ansible where
    validate Ansible{..} = do
        check aBin     "--bin must be specified."
        check aEnv     "--env must be specified."
        check aRetain  "--retention must be greater than 0."
        checkPath aKey " specified by --key must exist."

        check aArgs "Pass ansible options through using the -- delimiter.\n\
                    \Usage: khan ansible [KHAN OPTIONS] -- [ANSIBLE OPTIONS]."

instance Naming Ansible where
    names Ansible{..} = unversioned "base" aEnv

commands :: [Command]
commands =
    [ command inventory "inventory" "Output ansible compatible inventory."
        "Stuff."
    , command ansible "ansible" "Ansible."
        "Stuff."
    , command playbook "playbook" "Ansible Playbook."
        "Stuff."
    ]

inventory :: Inventory -> AWS ()
inventory Inventory{..} = do
    i <- maybe list (const $ return Map.empty) iHost
    t <- render "inventory.mustache" $ sections i
    debug "Writing inventory to {}" [iCache]
    liftIO $ LBS.putStrLn (Aeson.encodePretty i)
    liftIO $ LBS.writeFile (Path.encodeString iCache) t
  where
    list = EC2.findInstances [] [Filter ("tag:" <> envTag) [iEnv]] >>=
        foldlM attrs Map.empty

    attrs m RunningInstancesItemType{..} = case riitDnsName of
        Nothing  -> return m
        Just dns -> do
            Tags{..} <- lookupTags $ tags riitTagSet
            let Names{..} = createNames tagRole tagEnv tagVersion
                upd m' k  = Map.insertWith (<>) k (Set.singleton dns) m'
            return $ foldl' upd m [roleName, appName, imageName]

    tags = map (\ResourceTagSetItemType{..} -> (rtsitKey, rtsitValue))

    sections = Map.singleton ("sections" :: Text) . map vars . Map.toList

    vars (k, vs) = Map.fromList
        [ ("name" :: Text, Aeson.toJSON k)
        , ("values", Aeson.toJSON vs)
        ]

ansible :: Ansible -> AWS ()
ansible Ansible{..} = do
    whenM ((|| aForce) <$> exceeds) $ do
        log "Limit of {}s exceeded for {}, refreshing..." [show aRetain, inv]
        inventory $ Inventory aEnv True Nothing aCache True
    log "ansible {}" [intercalate " " args]
    liftIO $ Posix.executeFile bin True args Nothing
  where
    args = aArgs ++ foldr' add []
        [ ("-u", "ubuntu")
        , ("-i", inv)
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

    bin = Text.unpack $ fromMaybe "ansible" aBin
    inv = Path.encodeString aCache

playbook :: Ansible -> AWS ()
playbook a@Ansible{..} = ansible $ a
    { aBin = maybe (Just "ansible-playbook") Just aBin
    }
