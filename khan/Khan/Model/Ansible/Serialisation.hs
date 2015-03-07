{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

-- Module      : Khan.Model.Ansible.Serialisation
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Ansible.Serialisation where

import           Data.Aeson          as Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.Hashable
import qualified Data.Text.Lazy      as LText
import           GHC.Generics        (Generic)
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS

data Output
    = Change   LText.Text
    | NoChange LText.Text
    | Fail     LText.Text
      deriving (Show)

instance ToJSON Output where
    toJSON (Change   msg) = object ["changed" .= True,  "msg" .= msg]
    toJSON (NoChange msg) = object ["changed" .= False, "msg" .= msg]
    toJSON (Fail     msg) = object ["failed"  .= True,  "msg" .= msg]

data Inv a
    = Meta { unwrap :: !a }
    | JS   { unwrap :: !a }

deriving instance Eq  a => Eq (Inv a)
deriving instance Ord a => Ord (Inv a)

data Host
    = Host
      { hvFQDN   :: !Text
      , hvDomain :: !Text
      , hvNames  :: !Names
      , hvRegion :: !Region
      }
    | Localhost
      { hvRegion :: !Region
      } deriving (Eq, Ord, Generic)

instance Hashable Host

hostFQDN :: Host -> Text
hostFQDN Host      {..} = hvFQDN
hostFQDN Localhost {}   = "localhost"

instance ToJSON (Inv (HashMap Text (HashSet Host))) where
    toJSON (Meta m) = object ["_meta" .= object ["hostvars" .= vars]]
      where
        vars = foldl' (flip f) Map.empty . Set.unions $ Map.elems m
        f h  = Map.insert (hostFQDN h) (Meta h)

    toJSON (JS m) = toJSON (Map.map JS m) `f` toJSON (Meta m)
      where
        f (Object x) (Object y) = Object $ x <> y
        f _          x          = x

instance ToJSON (Inv (HashSet Host)) where
    toJSON i = case i of
        (Meta _) -> f Meta
        (JS   _) -> f JS
      where
        f x = toJSON . map x . Set.toList $ unwrap i

instance ToJSON (Inv Host) where
    toJSON (JS i) = String (hostFQDN i)

    toJSON (Meta Host      {..}) = object $
        ("khan_domain", String hvDomain) : hostVars hvRegion hvNames
    toJSON (Meta Localhost {..}) = object $
        -- Fix for https://github.com/brendanhay/khan/issues/74
        ("ansible_connection", String "local") : regionVars hvRegion

data ImageInput = ImageInput
    { iNames  :: !Names
    , iRegion :: !Region
    , iDNS    :: !Text
    }

instance ToJSON ImageInput where
    toJSON ImageInput{..} = object $
        ("khan_dns", String iDNS) : hostVars iRegion iNames

-- | Applied to all hosts, excluding localhost.
hostVars :: Region -> Names -> [(Text, Value)]
hostVars reg Names{..} = regionVars reg ++
    [ ("khan_env",     String envName)
    , ("khan_key",     String keyName)
    , ("khan_role",    String roleName)
    , ("khan_profile", String profileName)
    , ("khan_group",   String groupName)
    , ("khan_image",   String imageName)
    , ("khan_app",     String appName)
    , ("khan_version", toJSON versionName)
    ]

-- | Applied to all hosts, including localhost.
regionVars :: Region -> [(Text, Value)]
regionVars reg =
    [ ("khan_region",        String $ regionToText reg)
    , ("khan_region_abbrev", String $ abbreviate reg)
    ]

-- | Applied as --extra-vars arguments to ansible.
extraVars :: Names -> [(Text, Text)]
extraVars Names{..} =
    [ ("khan_env", envName)
    , ("khan_key", keyName)
    ]
