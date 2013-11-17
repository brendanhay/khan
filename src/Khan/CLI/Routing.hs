{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Khan.CLI.Routing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Routing (commands) where

import           Control.Arrow              ((&&&))
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Lazy.IO          as LText
import qualified Filesystem.Path.CurrentOS  as Path
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2            hiding (Instance, ec2)
import           Network.AWS.EC2.Metadata
import qualified Text.EDE                   as EDE

data Routes = Routes
    { rEnv      :: !Text
    , rDomain   :: Maybe Text
    , rRoles    :: [Text]
    , rZones    :: !String
    , rTemplate :: FilePath
    }

routesParser :: Parser Routes
routesParser = Routes
    <$> envOption
    <*> optional (textOption "domain" (short 'd')
        "DNS domain restriction.")
    <*> many (textOption "role" (short 'r')
        "Role to restrict to.")
    <*> stringOption "zones" (value "")
        "Availability zones suffixes restriction."
    <*> pathOption "template" (action "file" <> value "" <> short 't')
        "Path to the ED-E HAProxy configuration template."

instance Options Routes where
    discover ec2 r@Routes{..} = do
        zs <- EC2.defaultZoneSuffixes rZones
        f  <- if invalid rTemplate then configPath "haproxy.ede" else return rTemplate
        debug "Using Availability Zones '{}'" [zs]
        if not ec2
            then return $! r { rZones = zs, rTemplate = f }
            else do
                iid <- liftEitherT $ Text.decodeUtf8 <$> metadata InstanceId
                Tags{..} <- findRequiredTags iid
                return $! r
                    { rDomain   = Just tagDomain
                    , rEnv      = tagEnv
                    , rZones    = zs
                    , rTemplate = f
                    }

    validate Routes{..} = do
        check rZones "--zones must be specified."
        checkPath rTemplate " specified by --template must exist."

commands :: Mod CommandFields Command
commands = command "routes" routes routesParser
    "Generate a routing table using the specified filters and environment."

routes :: Common -> Routes -> AWS ()
routes Common{..} r@Routes{..} = do
    is <- EC2.findInstances [] $ filters cRegion r
    f  <- liftIO . LText.readFile $ Path.encodeString rTemplate

    let xs = map mkInstance $ filter (isJust . riitDnsName) is
        ys = Map.fromListWith (<>) [(k, [v]) | (k, v) <- xs]
        zs = EDE.fromPairs ["roles" .= ys]

--    liftIO . LBS.putStrLn $ Aeson.encodePretty zs
    liftIO . either print (LText.putStrLn . EDE.toLazyText) $
        EDE.eitherParse f >>= EDE.eitherRender zs

filters :: Region -> Routes -> [Filter]
filters reg Routes{..} = catMaybes
    [ Just $ Filter "availability-zone" zones
    , Just $ Filter ("tag:" <> envTag) [rEnv]
    , fmap (Filter ("tag:" <> domainTag) . (:[])) rDomain
    , role
    ]
  where
    zones = map (Text.pack . show . AZ reg) rZones

    role | [] <- rRoles = Nothing
         | otherwise    = Just $ Filter ("tag:" <> roleTag) rRoles

mkInstance :: RunningInstancesItemType -> (Text, Object)
mkInstance RunningInstancesItemType{..} = (role,) $ EDE.fromPairs
    [ "instanceId"       .= riitInstanceId
    , "imageId"          .= riitImageId
    , "instanceState"    .= istName riitInstanceState
    , "stateReason"      .= fmap srtMessage riitStateReason
    , "instanceType"     .= riitInstanceType
    , "dnsName"          .= riitDnsName
    , "privateDnsName"   .= riitPrivateDnsName
    , "ipAddress"        .= riitIpAddress
    , "privateIpAddress" .= riitPrivateIpAddress
    , "availabilityZone" .= pruAvailabilityZone riitPlacement
    , "domain"           .= lookup domainTag tags
    , "name"             .= lookup nameTag tags
    , "version"          .= lookup versionTag tags
    , "role"             .= role
    ]
  where
    tags = map (rtsitKey &&& rtsitValue) riitTagSet
    role = fromJust $ lookup roleTag tags
