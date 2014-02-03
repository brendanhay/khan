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
    -- * EC2 Filter
      filter

    -- * Constants
    , env
    , role
    , domain
    , name
    , version
    , weight

    -- * Defaults
    , defaults

    -- * From EC2 instance
    , flatten

    -- * Lookup from HashMap
    , lookup
    , lookupVersion
    , lookupWeight

    -- * API calls
    , required
    , apply
    ) where

import           Control.Monad.Error
import qualified Data.Attoparsec.Text  as AText
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as Map
import           Data.SemVer
import qualified Data.Text             as Text
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Prelude          hiding (filter, lookup)
import           Network.AWS
import           Network.AWS.EC2

filter :: Text -> [Text] -> Filter
filter k = Filter ("tag:" <> k)

env, role, domain, name, version, weight :: Text
env     = "Env"
role    = "Role"
domain  = "Domain"
name    = "Name"
version = "Version"
weight  = "Weight"

defaults :: Names -> Text -> [(Text, Text)]
defaults Names{..} dom =
    [ (role,   roleName)
    , (env,    envName)
    , (domain, dom)
    , (name,   appName)
    , (weight, "0")
    ] ++ maybe [] (\v -> [(version, v)]) versionName

flatten :: [ResourceTagSetItemType] -> HashMap Text Text
flatten = Map.fromList
    . map (\ResourceTagSetItemType{..} -> (rtsitKey, rtsitValue))

lookup :: (Applicative m, MonadError AWSError m) => HashMap Text Text -> m Tags
lookup ts = Tags
    <$> (Role <$> require role ts)
    <*> (Env <$> require env ts)
    <*> require domain ts
    <*> pure (Map.lookup name ts)
    <*> pure (lookupVersion ts)
    <*> pure (lookupWeight ts)
  where
    require k m = hoistError . note (missing k m) $ Map.lookup k m

    missing k m = Err
        . Text.unpack
        $ Text.concat ["No tag '", k, "' found in [", render m, "]"]

    render = Text.intercalate ","
        . map (\(k, v) -> Text.concat [k, "=", v])
        . Map.toList

lookupVersion :: HashMap Text Text -> Maybe Version
lookupVersion = join
    . fmap (hush . parseVersion)
    . Map.lookup version

lookupWeight :: HashMap Text Text -> Int
lookupWeight = fromMaybe 0
    . join
    . fmap (hush . AText.parseOnly AText.decimal)
    . Map.lookup weight

required :: Text -> AWS Tags
required iid = do
    log "Describing tags for instance-id {}..." [iid]
    send (DescribeTags [TagResourceId [iid]]) >>= lookup . tags
  where
    tags = Map.fromList
        . map (\TagSetItemType{..} -> (tsitKey, tsitValue))
        . dtagsrTagSet

apply :: Naming a => a -> Text -> [Text] -> AWS ()
apply (names -> n) dom ids = do
    log_ "Tagging instances..."
    send_ . CreateTags ids
          . map (uncurry ResourceTagSetItemType)
          $ defaults n dom