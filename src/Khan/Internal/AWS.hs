{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Internal.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.AWS where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import qualified Data.Map                as Map
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Version
import           Khan.Internal.Log
import           Khan.Internal.Types
import           Network.AWS
import           Network.AWS.AutoScaling hiding (DescribeTags)
import           Network.AWS.EC2
import           Network.AWS.IAM

policyPath :: FilePath
policyPath = "./config/role-policy.json"

trustPath :: FilePath
trustPath = "./config/trust-relationship.json"

sshGroup :: Text -> Text
sshGroup = (<> "-ssh")

sshRules :: [IpPermissionType]
sshRules = [IpPermissionType TCP 22 22 [] [IpRange "0.0.0.0/0"]]

certPath :: FilePath
certPath = "./cert"

envTag, roleTag, domainTag, nameTag, versionTag :: Text
envTag     = "Env"
roleTag    = "Role"
domainTag  = "Domain"
nameTag    = "Name"
versionTag = "Version"

defaultTags :: Names -> Text -> [(Text, Text)]
defaultTags Names{..} dom =
    [ (roleTag, roleName)
    , (envTag, envName)
    , (domainTag, dom)
    ] ++ maybe [] (\v -> [(versionTag, v)]) versionName

data Tags = Tags
    { tagRole    :: !Text
    , tagEnv     :: !Text
    , tagDomain  :: !Text
    , tagVersion :: Maybe Version
    }

requiredTags :: Text -> AWS Tags
requiredTags iid = do
    logInfo "Describing tags for instance-id {}..." [iid]
    ts <- toMap <$> send (DescribeTags [TagResourceId [iid]])
    Tags <$> require roleTag ts
         <*> require envTag ts
         <*> require domainTag ts
         <*> pure (join $ parse <$> Map.lookup versionTag ts)
  where
    toMap = Map.fromList
        . map (\TagSetItemType{..} -> (tsitKey, tsitValue))
        . dtagsrTagSet

    require k m = noteError (message k m) $ Map.lookup k m

    message k m = Text.unpack $
        Text.concat ["No tag '", k, "' found in [", render m, "]"]

    render = Text.intercalate ","
        . map (\(k, v) -> Text.concat [k, "=", v])
        . Map.toList

    parse = hush . parseVersionE . Text.unpack

verifyAS :: Text -> Either AutoScalingErrorResponse a -> AWS ()
verifyAS  = (`verify` (aseCode . aserError))

verifyEC2 :: Text -> Either EC2ErrorResponse a -> AWS ()
verifyEC2 = (`verify` (ecCode . head . eerErrors))

verifyIAM :: Text -> Either IAMError a -> AWS ()
verifyIAM = (`verify` (etCode . erError))

verify :: (Eq a, ToError e) => a -> (e -> a) -> Either e b -> AWS ()
verify k f = checkError ((k ==) . f)
