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

import           Data.Aeson
import qualified Data.HashMap.Strict             as Map
import           Data.String
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.IO               as LText
import qualified Filesystem.Path.CurrentOS       as Path
import           Khan.Internal
import qualified Khan.Model.EC2.AvailabilityZone as AZ
import qualified Khan.Model.EC2.Instance         as Instance
import qualified Khan.Model.Tag                  as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2                 hiding (Instance, ec2)
import qualified Text.EDE                        as EDE

data Routes = Routes
    { rEnv      :: !Env
    , rDomain   :: Maybe Text
    , rRoles    :: [Text]
    , rZones    :: !String
    , rWeight   :: !Int
    , rTemplate :: !FilePath
    }

routesParser :: EnvMap -> Parser Routes
routesParser env = Routes
    <$> envOption env
    <*> optional (textOption "domain" (short 'd')
        "DNS domain restriction.")
    <*> many (textOption "role" (short 'r')
        "Role to restrict to.")
    <*> stringOption "zones" (value "")
        "Availability zones suffixes restriction."
    <*> integralOption "min-weight" (value 1)
        "Minimum weight restriction."
    <*> pathOption "template" (action "file" <> value "" <> short 't')
        "Path to the ED-E HAProxy configuration template."

instance Options Routes where
    discover aws Common{..} r@Routes{..} = do
        zs <- AZ.getSuffixes rZones
        debug "Using Availability Zones '{}'" [zs]
        if not aws
            then return $! r { rZones = zs, rTemplate = f }
            else do
                iid      <- liftEitherT $ meta InstanceId
                Tags{..} <- Tag.require iid
                return $! r
                    { rDomain   = Just tagDomain
                    , rEnv      = tagEnv
                    , rZones    = zs
                    , rTemplate = f
                    }
      where
        f = if invalid rTemplate
                then configDir cConfig </> Path.decodeString "haproxy.ede"
                else rTemplate

    validate Routes{..} = do
        check rEnv   "--env must be specified."
        check rZones "--zones must be specified."
        checkPath rTemplate " specified by --template must exist."

commands :: EnvMap -> Mod CommandFields Command
commands env = command "routes" routes (routesParser env)
    "Generate a routing table using the specified filters and environment."

routes :: Common -> Routes -> AWS ()
routes Common{..} Routes{..} = do
    reg <- getRegion
    is  <- mapMaybe Tag.annotate <$> Instance.findAll [] (filters reg)

    let xs = [ mk a | a@Ann{..} <- is
             , tagWeight annTags >= rWeight
             , isJust (Instance.address cVPN annValue)
             ]
        ys = Map.fromListWith (<>) [(k, [v]) | (k, v) <- xs]
        zs = EDE.fromPairs ["roles" .= ys]

    renderTemplate zs rTemplate >>= liftIO . LText.putStrLn
  where
    filters reg = catMaybes
        [ Just . Filter "availability-zone" $ zones reg
        , Just $ Tag.filter Tag.env [_env rEnv]
        , fmap (Tag.filter Tag.domain . (:[])) rDomain
        , role
        ]

    zones reg = map (Text.pack . show . AZ reg) rZones

    role | [] <- rRoles = Nothing
         | otherwise    = Just (Tag.filter Tag.role rRoles)

    mk Ann{..} =
        let RunningInstancesItemType{..} = annValue
            Tags{..}                     = annTags
         in (_role tagRole,) $ EDE.fromPairs
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
                , "env"              .= tagEnv
                , "role"             .= tagRole
                , "domain"           .= tagDomain
                , "name"             .= tagName
                , "version"          .= tagVersion
                , "weight"           .= tagWeight
                ]
