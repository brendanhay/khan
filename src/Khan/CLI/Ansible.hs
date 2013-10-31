{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

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

import           Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as Map
import           Data.HashMap.Strict        (HashMap)
import           Data.List                  (intercalate)
import qualified Data.Set                   as Set
import           Data.Set                   (Set)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Encoding    as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.EC2
import           System.Directory
import qualified System.Posix.Files         as Posix
import qualified System.Posix.Process       as Posix

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
        c <- defaultPath iCache . cachePath $ Path.fromText iEnv
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
        c <- defaultPath aCache . cachePath $ Path.fromText aEnv
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
    inv <- maybe list (const $ return Map.empty) iHost
    t   <- render "inventory.mustache" $ INI inv
    debug "Writing inventory to {}" [iCache]
    liftIO $ LBS.writeFile (Path.encodeString iCache) t
    unless iSilent . liftIO . LBS.putStrLn . Aeson.encodePretty $ JS inv
  where
    list = EC2.findInstances [] [Filter ("tag:" <> envTag) [iEnv]] >>=
        foldlM hosts Map.empty

    hosts m RunningInstancesItemType{..} = case riitDnsName of
        Nothing   -> return m
        Just fqdn -> do
            reg      <- Text.pack . show <$> getRegion
            Tags{..} <- lookupTags $ map tag riitTagSet

            let n@Names{..} = createNames tagRole tagEnv tagVersion
                host        = Host fqdn tagDomain n reg
                update k    = Map.insertWith (<>) k (Set.singleton host)

            return $! foldl' (flip update) m
                [ roleName
                , envName
                , reg
                , Text.concat [envName, "-", reg]
                ]

    tag ResourceTagSetItemType{..} = (rtsitKey, rtsitValue)

ansible :: Ansible -> AWS ()
ansible Ansible{..} = do
    whenM ((|| aForce) <$> exceeds) $ do
        log "Limit of {}s exceeded for {}, refreshing..." [show aRetain, inv]
        inventory $ Inventory aEnv True Nothing aCache True
    log "ansible {}" [intercalate " " args]
    liftIO $ Posix.executeFile bin True args Nothing
  where
    args = aArgs ++ foldr' add []
        [ ("-i", inv)
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

data Format a
    = Meta { unwrap :: a }
    | JS   { unwrap :: a }
    | INI  { unwrap :: a }

deriving instance Eq  a => Eq (Format a)
deriving instance Ord a => Ord (Format a)

data Host = Host
    { hvFQDN   :: !Text
    , hvDomain :: !Text
    , hvNames  :: !Names
    , hvRegion :: !Text
    } deriving (Eq, Ord)

instance ToJSON (Format (HashMap Text (Set Host))) where
    toJSON (Meta m) = object ["_meta" .= object ["hostvars" .= Map.map Meta m]]
    toJSON (JS   m) = toJSON (Map.map JS m) `mappendJSON` toJSON (Meta m)
    toJSON (INI  m) = object
        [ "sections" .= map
            (\(k, vs) -> object ["name" .= k, "values" .= INI vs])
            (Map.toList m)
        ]

instance ToJSON (Format (Set Host)) where
    toJSON (Meta s) = toJSON $ Set.map Meta s
    toJSON (JS   s) = toJSON $ Set.map JS s
    toJSON (INI  s) = toJSON $ Set.map INI s

instance ToJSON (Format Host) where
    toJSON x = case x of
        (Meta _) -> object vars
        (JS   _) -> pack hvFQDN
        (INI  _) -> pack host
      where
        Host{..}  = unwrap x
        Names{..} = hvNames

        host = Text.concat . (hvFQDN :) $ concatMap (uncurry text) vars

        text k (Aeson.String v) = [" ", k, "=", v]
        text _ _                = []

        pack = Aeson.String

        vars = [ ("khan_region",  pack hvRegion)
               , ("khan_domain",  pack hvDomain)
               , ("khan_env",     pack envName)
               , ("khan_key",     pack keyName)
               , ("khan_role",    pack roleName)
               , ("khan_profile", pack profileName)
               , ("khan_group",   pack groupName)
               , ("khan_image",   pack imageName)
               , ("khan_app",     pack appName)
               , ("khan_version", toJSON versionName)
               ]

mappendJSON :: Value -> Value -> Value
mappendJSON (Object x) (Object y) = Object $ x <> y
mappendJSON _          x          = x
