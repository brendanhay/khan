{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
    , cached
    , require
    , instances
    , images
    , defaults
    ) where

import           Control.Monad.Except
import qualified Data.Attoparsec.Text  as AText
import qualified Data.HashMap.Strict   as Map
import qualified Data.SemVer           as Ver
import qualified Data.Text             as Text
import           Data.Text.Lazy.IO     as LText
import           Filesystem            as FS
import           Khan.Internal         hiding (group)
import           Khan.Model.Tag.Tagged
import           Khan.Prelude          hiding (filter)
import           Network.AWS
import           Network.AWS.EC2

default (Text)

annotate :: Tagged a => a -> Maybe (Ann a)
annotate x = Ann x <$> hush (parse x)

parse :: Tagged a => a -> Either String Tags
parse (tags -> ts) = Tags
    <$> (newRole <$> key role)
    <*> (newEnv <$> key env)
    <*> key domain
    <*> opt (key name)
    <*> lookupVersion
    <*> lookupWeight
    <*> opt (key group)
  where
    lookupVersion = opt $
        key version >>= Ver.fromText

    lookupWeight = maybe (Right 0) Right . hush $
        key weight >>= AText.parseOnly AText.decimal

    key k = note ("Failed to find key: " ++ Text.unpack k) (Map.lookup k ts)

    opt (Left  _) = Right Nothing
    opt (Right x) = Right (Just x)

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

cached :: CacheDir -> Text -> AWS Tags
cached (CacheDir dir) iid = do
    say "Lookup cached tags from {} for Instance {}..." [B path, B iid]
    r <- load
    either (\e -> say "Error reading cached tags: {}" [e] >> store)
           return
           r
  where
    path = dir </> ".tags"

    store = do
        ts <- require iid
        liftIO . FS.withTextFile path WriteMode $ \hd ->
            LText.hPutStr hd (renderEnv True ts)
        return ts

    load = liftIO $ do
        FS.isFile path >>=
            bool (return (Left "Missing cached .tags"))
                 (parse . hashMap <$> FS.readTextFile path)

      where
        hashMap = Map.fromList . mapMaybe split . Text.lines

        split x =
            case Text.split (== '=') x of
                [k, v] -> Just (strip k, v)
                _      -> Nothing

        strip x = Text.toUpper . fromMaybe x $ Text.stripPrefix "KHAN_" x

require :: Text -> AWS Tags
require iid = do
    say "Describing tags for Instance {}..." [iid]
    parse <$> send (DescribeTags [TagResourceId [iid]]) >>=
        liftEitherT . hoistEither

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
