{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

-- Module      : Khan.Model.Ansible.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Ansible.Internal where

import           Data.Aeson            as Aeson
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as LText
import           Khan.Internal.AWS
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS

data Output
    = Changed   !LText.Text
    | Unchanged !LText.Text
    | Failed    !LText.Text
      deriving (Show)

instance ToJSON Output where
    toJSON (Changed   msg) = object ["changed" .= True,  "msg" .= msg]
    toJSON (Unchanged msg) = object ["changed" .= False, "msg" .= msg]
    toJSON (Failed    msg) = object ["failed"  .= True,  "msg" .= msg]

data Inv a
    = Meta { unwrap :: a }
    | JS   { unwrap :: a }

deriving instance Eq  a => Eq (Inv a)
deriving instance Ord a => Ord (Inv a)

data Host = Host
    { hvFQDN   :: !Text
    , hvDomain :: !Text
    , hvNames  :: !Names
    , hvRegion :: !Region
    } deriving (Eq, Ord)

instance ToJSON (Inv (HashMap Text (Set Host))) where
    toJSON (Meta m) = object ["_meta" .= object ["hostvars" .= vars]]
      where
        vars = foldl' (flip f) Map.empty . Set.unions $ Map.elems m
        f h  = Map.insert (hvFQDN h) (Meta h)

    toJSON (JS m) = toJSON (Map.map JS m) `f` toJSON (Meta m) `f` local
      where
        f (Object x) (Object y) = Object $ x <> y
        f _          x          = x

        local = object ["localhost" .= ["localhost" :: Text]]

instance ToJSON (Inv (Set Host)) where
    toJSON x = case x of
        (Meta _) -> f Meta
        (JS   _) -> f JS
      where
        f c = toJSON . map c . Set.toList $ unwrap x

instance ToJSON (Inv Host) where
    toJSON x = case x of
        (JS   _) -> String hvFQDN
        (Meta _) -> object $
            ("khan_domain", String hvDomain) : variables hvRegion hvNames
      where
        Host{..} = unwrap x

data ImageInput = ImageInput
    { iNames  :: !Names
    , iRegion :: !Region
    , iDNS    :: !Text
    }

instance ToJSON ImageInput where
    toJSON ImageInput{..} =
        object $ ("khan_dns", String iDNS) : variables iRegion iNames

variables :: Region -> Names -> [(Text, Value)]
variables reg Names{..} =
    [ ("khan_region",        String . Text.pack $ show reg)
    , ("khan_region_abbrev", String $ abbreviate reg)
    , ("khan_env",           String envName)
    , ("khan_key",           String keyName)
    , ("khan_role",          String roleName)
    , ("khan_profile",       String profileName)
    , ("khan_group",         String groupName)
    , ("khan_image",         String imageName)
    , ("khan_app",           String appName)
    , ("khan_version",       toJSON versionName)
    ]
