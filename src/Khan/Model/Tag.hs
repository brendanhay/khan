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
    -- * Constants
    , envTag
    , roleTag
    , domainTag
    , nameTag
    , versionTag
    , weightTag

    -- * Defaults
    , defaultTags

    -- * Lookup from HashMap
    , lookupTags
    , lookupVersionTag
    , lookupWeightTag

    -- * API calls
    , findRequiredTags
    ) where

import           Control.Monad.Error
import qualified Data.Attoparsec.Text     as AText
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.SemVer
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Data.Text.Format         (Format, format)
import           Data.Text.Format.Params
import qualified Data.Text.Lazy           as LText
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Prelude             hiding (min, max)
import           Network.AWS
import           Network.AWS.AutoScaling  hiding (DescribeTags)
import           Network.AWS.EC2          as EC2
import           Network.AWS.EC2.Metadata (Meta(..))
import qualified Network.AWS.EC2.Metadata as Meta
import           Network.AWS.IAM

envTag, roleTag, domainTag, nameTag, versionTag, weightTag :: Text
envTag     = "Env"
roleTag    = "Role"
domainTag  = "Domain"
nameTag    = "Name"
versionTag = "Version"
weightTag  = "Weight"

defaultTags :: Names -> Text -> [(Text, Text)]
defaultTags Names{..} dom =
    [ (roleTag,   roleName)
    , (envTag,    envName)
    , (domainTag, dom)
    , (nameTag,   appName)
    , (weightTag, "0")
    ] ++ maybe [] (\v -> [(versionTag, v)]) versionName

lookupTags :: (Applicative m, MonadError AWSError m) => [(Text, Text)] -> m Tags
lookupTags (Map.fromList -> ts) = Tags
    <$> require roleTag ts
    <*> require envTag ts
    <*> require domainTag ts
    <*> pure (lookupVersionTag ts)
    <*> pure (lookupWeightTag ts)
  where
    require k m = hoistError . note (missing k m) $ Map.lookup k m

    missing k m = Err
        . Text.unpack
        $ Text.concat ["No tag '", k, "' found in [", render m, "]"]

    render = Text.intercalate ","
        . map (\(k, v) -> Text.concat [k, "=", v])
        . Map.toList

lookupVersionTag :: HashMap Text Text -> Maybe Version
lookupVersionTag = join
    . fmap (hush . parseVersion)
    . Map.lookup versionTag

lookupWeightTag :: HashMap Text Text -> Int
lookupWeightTag = fromMaybe 0
    . join
    . fmap (hush . AText.parseOnly AText.decimal)
    . Map.lookup weightTag

findRequiredTags :: Text -> AWS Tags
findRequiredTags iid = do
    log "Describing tags for instance-id {}..." [iid]
    send (DescribeTags [TagResourceId [iid]]) >>= lookupTags . tags
  where
    tags = map (\TagSetItemType{..} -> (tsitKey, tsitValue)) . dtagsrTagSet
