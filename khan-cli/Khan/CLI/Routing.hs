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
    , rZones    :: Maybe [Char]
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
    <*> optional (stringOption "zones" mempty
        "Availability zones suffixes restriction.")
    <*> integralOption "min-weight" (value 1)
        "Minimum weight restriction."
    <*> pathOption "template" (action "file" <> value "" <> short 't')
        "Path to the ED-E HAProxy configuration template."

instance Options Routes where
    discover aws Common{..} r@Routes{..} = do
        mzs <- zones rZones
        if aws
            then tags mzs
            else return $! r { rZones = mzs, rTemplate = tmpl }
      where
        zones Nothing   = return Nothing
        zones (Just xs) = do
            zs <- AZ.getSuffixes xs
            debug "Using Availability Zones '{}'" [zs]
            return (Just zs)

        tags mzs = do
            iid      <- liftEitherT $ meta InstanceId
            Tags{..} <- Tag.require iid
            return $! r
                { rDomain   = Just tagDomain
                , rEnv      = tagEnv
                , rZones    = mzs
                , rTemplate = tmpl
                }

        tmpl | valid rTemplate = rTemplate
             | otherwise       =
                 configDir cConfig </> Path.decodeString "haproxy.ede"

    validate Routes{..} = do
        check rEnv   "--env must be specified."
        check rZones "--zones must be specified."
        checkFile rTemplate " specified by --template must exist."

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
    filters r = catMaybes
        [ Just $ Tag.filter Tag.env [_env rEnv]
        , fmap (Tag.filter Tag.domain . (:[])) rDomain
        , fmap (Filter "availability-zone") (zones r)
        , role
        ]

    zones r = map (Text.pack . show . AZ r) <$> rZones

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
