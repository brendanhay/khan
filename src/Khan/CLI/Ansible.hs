{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

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

import           Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.EC2

defineOptions "Inventory" $ do
    textOption "iDomain" "domain" ""
        "DNS domain. (required)"

    textOption "iEnv" "env" defaultEnv
        "Environment."

    boolOption "iList" "list" True
        "List."

    maybeTextOption "iHost" "host" ""
        "Host."

deriving instance Show Inventory

instance Discover Inventory

instance Validate Inventory where
    validate Inventory{..} = do
        check iEnv    "--env must be specified."
        check iDomain "--domain must be specified."

commands :: [Command]
commands =
    [ command inventory "inventory" "Output ansible compatible inventory"
        "Stuff."
    ]

inventory :: Inventory -> AWS ()
inventory Inventory{..} =
    maybe list (const $ return "{}") iHost >>= liftIO . LBS.putStrLn
  where
    list = EC2.findInstances [] filters >>=
        fmap Aeson.encodePretty . foldlM attrs Map.empty

    filters =
        [ Filter ("tag:" <> envTag)    [iEnv]
        , Filter ("tag:" <> domainTag) [iDomain]
        ]

    attrs m RunningInstancesItemType{..} = do
        Tags{..} <- lookupTags $ tags riitTagSet
        let Names{..} = createNames tagRole tagEnv tagVersion
            upd m' k   = Map.insertWith (<>) k (Set.singleton riitDnsName) m'
        return $ foldl' upd m [roleName]

    tags = map (\ResourceTagSetItemType{..} -> (rtsitKey, rtsitValue))
