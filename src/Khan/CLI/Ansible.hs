{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (intercalate)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Format           as Format
import qualified Data.Text.Lazy.IO          as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.EC2
import           System.Directory
import qualified System.Posix.Files         as Posix
import qualified System.Posix.Process       as Posix

data Ansible = Ansible
    { aEnv    :: !Text
    , aBin    :: Maybe Text
    , aKey    :: !FilePath
    , aRetain :: !Int
    , aCache  :: !FilePath
    , aForce  :: !Bool
    , aArgs   :: [String]
    }

ansibleParser :: Parser Ansible
ansibleParser = Ansible
    <$> envOption
    <*> optional (textOption "bin" mempty
        "Ansible binary name to exec.")
    <*> pathOption "key" (value "")
        "Path to the private key to use."
    <*> readOption "retention" "SECONDS" (value defaultCache)
        "Number of seconds to cache inventory results for."
    <*> pathOption "cache" (value "")
        "Path to the inventory file cache."
    <*> switchOption "force" False
        "Force update of any previously cached results."
    <*> argsOption str mempty
        "Pass through arugments to ansible."

instance Options Ansible where
    discover _ a@Ansible{..} = do
        f <- if invalid aKey then keyPath $ names a else return aKey
        c <- inventoryPath aCache aEnv
        return $! a { aKey = f, aCache = c }

    validate Ansible{..} = do
        check aBin     "--bin must be specified."
        check aEnv     "--env must be specified."
        check aRetain  "--retention must be greater than 0."
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
    <*> pathOption "cache" (value "")
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

    validate Inventory{..} =
        check iEnv "--env must be specified."

commands :: Mod CommandFields Command
commands =
       command "ansible" ansible ansibleParser
       "Ansible."
    <> command "playbook" playbook ansibleParser
       "Ansible Playbook."
    <> command "inventory" inventory inventoryParser
       "Output ansible compatible inventory."

ansible :: Common -> Ansible -> AWS ()
ansible cmn Ansible{..} = do
    whenM ((|| aForce) <$> exceeds) $ do
        log "Limit of {}s exceeded for {}, refreshing..." [show aRetain, inv]
        inventory cmn $ Inventory aEnv aCache True True Nothing

    debug "Writing inventory script to {}" [script]
    liftIO . LText.writeFile script $
        Format.format "#!/usr/bin/env bash\nset -e\nexec cat {}\n" [inv]

    debug "Setting +rwx on {}" [script]
    liftIO $ Posix.setFileMode script Posix.ownerModes

    log "{} {}" [bin, intercalate " " args]
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
playbook cmn a@Ansible{..} = do
    r <- show <$> getRegion
    ansible cmn $ a
        { aBin  = maybe (Just "ansible-playbook") Just aBin
        , aArgs = aArgs ++
            [ "--extra-vars"
            , concat ["khan_region=", r, " khan_env=", Text.unpack aEnv]
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
    list = EC2.findInstances [] [Filter ("tag:" <> envTag) [iEnv]] >>=
        foldlM hosts Map.empty

    hosts m RunningInstancesItemType{..} = case riitDnsName of
        Nothing   -> return m
        Just fqdn -> do
            reg      <- Text.pack . show <$> getRegion
            Tags{..} <- lookupTags $ map tag riitTagSet

            let n@Names{..} = createNames tagRole tagEnv tagVersion
                host     = Host fqdn tagDomain n reg
                update k = Map.insertWith (<>) k (Set.singleton host)

            return $! foldl' (flip update) m
                [roleName, envName, reg, "khan", tagDomain]

    tag ResourceTagSetItemType{..} = (rtsitKey, rtsitValue)

data Format a
    = Meta { unwrap :: a }
    | JS   { unwrap :: a }

deriving instance Eq  a => Eq (Format a)
deriving instance Ord a => Ord (Format a)

data Host = Host
    { hvFQDN   :: !Text
    , hvDomain :: !Text
    , hvNames  :: !Names
    , hvRegion :: !Text
    } deriving (Eq, Ord)

instance ToJSON (Format (HashMap Text (Set Host))) where
    toJSON (Meta m) = object ["_meta" .= object ["hostvars" .= vars]]
      where
        vars = foldl' (flip f) Map.empty . Set.unions $ Map.elems m
        f h  = Map.insert (hvFQDN h) (Meta h)

    toJSON (JS m) = toJSON (Map.map JS m) `f` toJSON (Meta m) `f` local
      where
        local = object ["localhost" .= ["localhost" :: Text]]

        f (Object x) (Object y) = Object $ x <> y
        f _          x          = x

instance ToJSON (Format (Set Host)) where
    toJSON x = case x of
        (Meta _) -> f Meta
        (JS   _) -> f JS
      where
        f c = toJSON . map c . Set.toList $ unwrap x

instance ToJSON (Format Host) where
    toJSON x = case x of
        (Meta _) -> object vars
        (JS   _) -> pack hvFQDN
      where
        Host{..}  = unwrap x
        Names{..} = hvNames

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

inventoryPath :: FilePath -> Text -> AWS FilePath
inventoryPath f env = do
    r <- Text.pack . show <$> getRegion
    defaultPath f
        . cachePath
        . Path.fromText
        $ Text.concat [r, "_", env]
