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

    -- * Defaults
    , defaults

    -- * Tag filters
    , filter

    -- * API calls
    , required
    , apply
    ) where

import           Control.Monad.Error
import qualified Data.Attoparsec.Text    as AText
import qualified Data.HashMap.Strict     as Map
import           Data.SemVer
import qualified Data.Text               as Text
import           Khan.Internal           hiding (group)
import           Khan.Model.Tag.Internal
import           Khan.Prelude            hiding (filter)
import           Network.AWS
import           Network.AWS.EC2

annotate :: (Applicative m, MonadError AWSError m, Tagged a) => a -> m (Ann a)
annotate x = Ann x <$> parse x

parse :: (Applicative m, MonadError AWSError m, Tagged a) => a -> m Tags
parse (tags -> ts) = Tags
    <$> (newRole <$> require role ts)
    <*> (newEnv  <$> require env ts)
    <*> require domain ts
    <*> pure (Map.lookup name ts)
    <*> pure (lookupVersion ts)
    <*> pure (lookupWeight ts)
    <*> pure (Map.lookup group ts)
  where
    require k m = hoistError . note (missing k m) $ Map.lookup k m

    missing k m = Err
        . Text.unpack
        $ Text.concat ["No tag '", k, "' found in [", render m, "]"]

    render = Text.intercalate ","
        . map (\(k, v) -> Text.concat [k, "=", v])
        . Map.toList

    lookupVersion = join
        . fmap (hush . parseVersion)
        . Map.lookup version

    lookupWeight = fromMaybe 0
        . join
        . fmap (hush . AText.parseOnly AText.decimal)
        . Map.lookup weight

env, role, domain, name, version, weight, group :: Text
env     = "Env"
role    = "Role"
domain  = "Domain"
name    = "Name"
version = "Version"
weight  = "Weight"
group   = "aws:autoscaling:groupName"

defaults :: Names -> Text -> [(Text, Text)]
defaults Names{..} dom =
    [ (role,   roleName)
    , (env,    envName)
    , (domain, dom)
    , (name,   appName)
    , (weight, "0")
    ] ++ maybe [] (\v -> [(version, v)]) versionName

filter :: Text -> [Text] -> Filter
filter k = ec2Filter ("tag:" <> k)

required :: Text -> AWS Tags
required iid = do
    say "Describing tags for Instance {}..." [iid]
    send (DescribeTags [TagResourceId [iid]]) >>= parse

    -- tags = Map.fromList
    --     . map (\TagSetItemType{..} -> (tsitKey, tsitValue))
    --     . dtagsrTagSet

apply :: Naming a => a -> Text -> [Text] -> AWS ()
apply (names -> n) dom ids = do
    log_ "Tagging instances..."
    send_ . CreateTags ids
          . map (uncurry ResourceTagSetItemType)
          $ defaults n dom
