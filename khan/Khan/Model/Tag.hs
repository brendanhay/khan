{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.Tag
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Tag
    (
    -- * From EC2/ASG
      annotate
    , parse

    -- * Constants
    , env
    , role
    , domain
    , name
    , version
    , weight
    , group

    -- * Filters
    , filter

    -- * Tags
    , require
    , instances
    , images
    , defaults
    ) where

import           Control.Monad.Error
import qualified Data.Attoparsec.Text  as AText
import qualified Data.HashMap.Strict   as Map
import           Data.SemVer
import           Khan.Internal         hiding (group)
import           Khan.Model.Tag.Tagged
import           Khan.Prelude          hiding (filter)
import           Network.AWS
import           Network.AWS.EC2

annotate :: Tagged a => a -> Maybe (Ann a)
annotate x = Ann x <$> parse x

parse :: Tagged a => a -> Maybe Tags
parse (tags -> ts) = Tags
    <$> (newRole <$> key role)
    <*> (newEnv <$> key env)
    <*> key domain
    <*> pure (key name)
    <*> pure lookupVersion
    <*> pure lookupWeight
    <*> pure (key group)
  where
    key k = Map.lookup k ts

    lookupVersion = join
        . fmap (hush . parseVersion)
        $ Map.lookup version ts

    lookupWeight = fromMaybe 0
        . join
        . fmap (hush . AText.parseOnly AText.decimal)
        $ Map.lookup weight ts

env, role, domain, name, version, weight, group :: Text
env     = "Env"
role    = "Role"
domain  = "Domain"
name    = "Name"
version = "Version"
weight  = "Weight"
group   = "aws:autoscaling:groupName"

filter :: Text -> [Text] -> Filter
filter k = ec2Filter ("tag:" <> k)

require :: Text -> AWS Tags
require iid = do
    say "Describing tags for Instance {}..." [iid]
    parse <$> send (DescribeTags [TagResourceId [iid]]) >>=
        noteAWS "Unable to parse prerequisite tags for: {}" [iid]

instances :: Naming a => a -> Text -> [Text] -> AWS ()
instances n dom ids = do
    say "Tagging: {}" [L ids]
    send_ . CreateTags ids
          $ map (uncurry ResourceTagSetItemType) (defaults n dom)

images :: Naming a => a -> [Text] -> AWS ()
images (names -> Names{..}) ids = do
    say "Tagging: {}" [L ids]
    send_ $ CreateTags ids
        [ ResourceTagSetItemType role roleName
        , ResourceTagSetItemType version (fromMaybe "" versionName)
        , ResourceTagSetItemType name imageName
        ]

defaults :: Naming a => a -> Text -> [(Text, Text)]
defaults (names -> Names{..}) dom =
    [ (role,   roleName)
    , (env,    envName)
    , (domain, dom)
    , (name,   appName)
    , (weight, "0")
    ] ++ maybe [] (\v -> [(version, v)]) versionName
