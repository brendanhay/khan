{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Launch
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Launch (commands) where

import           Khan.Internal
import qualified Khan.Model.AvailabilityZone as AZ
import qualified Khan.Model.Image            as Image
import qualified Khan.Model.Instance         as Instance
import qualified Khan.Model.Key              as Key
import           Khan.Model.Role             (Paths(..))
import qualified Khan.Model.Role             as Role
import qualified Khan.Model.SecurityGroup    as Security
import qualified Khan.Model.Tag              as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2

-- FIXME:
-- Block Device Mappings
-- Monitoring
-- Disable Api Termination
-- Instance Shutdown Behavior
-- Client Token
-- Network Interfaces
data Launch = Launch
    { lRKeys     :: !RKeysBucket
    , lRole      :: !Role
    , lEnv       :: !Env
    , lDomain    :: !Text
    , lImage     :: Maybe Text
    , lNum       :: !Int
    , lGroups    :: [Text]
    , lType      :: !InstanceType
    , lOptimised :: !Bool
    , lZones     :: !String
    , lTrust     :: !TrustPath
    , lPolicy    :: !PolicyPath
    }

launchParser :: EnvMap -> Parser Launch
launchParser env = Launch
    <$> rKeysOption env
    <*> roleOption
    <*> envOption env
    <*> textOption "domain" (short 'd')
        "Instance's DNS domain."
    <*> optional (textOption "image" (value "")
        "Id of the image/ami.")
    <*> integralOption "num" (short 'n' <> value 1)
        "Number of instances to launch."
    <*> many (textOption "group" mempty
        "Security groups. (discovered)")
    <*> readOption "type" "TYPE" (value M1_Small)
        "Instance's type."
    <*> switchOption "optimised" False
        "EBS optimisation."
    <*> stringOption "zones" (value "")
        "Availability zones suffixes to provision into."
    <*> trustOption
    <*> policyOption

instance Options Launch where
    discover _ Common{..} l@Launch{..} = do
        zs <- AZ.getSuffixes lZones
        debug "Using Availability Zones '{}'" [zs]
        return $! l
            { lZones  = zs
            , lTrust  = pTrustPath
            , lPolicy = pPolicyPath
            }
      where
        Paths{..} = Role.paths l cConfig lTrust lPolicy

    validate Launch{..} = do
        check lEnv   "--env must be specified."
        check lZones "--zones must be specified."

        checkPath (_trust  lTrust)  " specified by --trust must exist."
        checkPath (_policy lPolicy) " specified by --policy must exist."

instance Naming Launch where
    names Launch{..} = unversioned lRole lEnv

commands :: EnvMap -> Mod CommandFields Command
commands env = mconcat
    [ command "launch" launch (launchParser env)
        "Launch 1..n instances."
    ]

launch :: Common -> Launch -> AWS ()
launch Common{..} l@Launch{..} = do
    log "Looking for Images matching {}" [fromMaybe imageName lImage]
    a <- async . Image.find [] . (:[]) $ maybe (Filter "name" [imageName])
        (Filter "image-id" . (:[])) lImage

    log "Looking for IAM Profiles matching {}" [profileName]
    p <- async $ Role.find l <|> Role.update l lTrust lPolicy

    ami <- diritImageId <$> wait a
    log "Using Image {}" [ami]

    wait_ p <* log "Found IAM Profile {}" [profileName]

    k <- async $ Key.create lRKeys l cLKeys
    s <- async $ Security.sshGroup l
    g <- async $ Security.create groupName

    wait_ k <* log "Found KeyPair {}" [keyName]
    wait_ s <* log "Found SSH Group {}" [sshGroupName]
    wait_ g <* log "Found Role Group {}" [groupName]

    az <- randomShuffle lZones
    rs <- forM (take lNum $ cycle az) $ \z ->
        Instance.run l ami lType (AZ cRegion z) 1 1 lOptimised

    let ids = concatMap (map riitInstanceId) rs

    Tag.apply l lDomain ids
    Instance.wait ids
  where
    Names{..} = names l
