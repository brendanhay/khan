{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Cluster
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Cluster (commands) where

import           Control.Concurrent          (threadDelay)
import           Data.Version
import           Khan.Internal
import qualified Khan.Model.AvailabilityZone as AZ
import qualified Khan.Model.Image            as AMI
import qualified Khan.Model.Key              as Key
import qualified Khan.Model.LaunchConfig     as Config
import qualified Khan.Model.Profile          as Profile
import qualified Khan.Model.ScalingGroup     as ASG
import qualified Khan.Model.SecurityGroup    as Security
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.AutoScaling hiding (Filter)
import           Network.AWS.EC2

data Deploy = Deploy
    { dRole     :: !Text
    , dEnv      :: !Text
    , dDomain   :: !Text
    , dVersion  :: !Version
    , dZones    :: !String
    , dGrace    :: !Integer
    , dMin      :: !Integer
    , dMax      :: !Integer
    , dDesired  :: !Integer
    , dCooldown :: !Integer
    , dType     :: !InstanceType
    }

-- FIXME: Add port(s) option, and apply to tags
deployParser :: Parser Deploy
deployParser = Deploy
    <$> roleOption
    <*> envOption
    <*> textOption "domain" (short 'd')
        "Instance's DNS domain."
    <*> versionOption
    <*> stringOption "zones" (value "")
         "Availability zones suffixes to provision into."
    <*> integerOption "grace" (value 20)
        "Seconds until healthchecks are activated."
    <*> integerOption "min" (value 1)
        "Minimum number of instances."
    <*> integerOption "max" (value 1)
        "Maximum number of instances."
    <*> integerOption "desired" (value 1)
        "Desired number of instances."
    <*> integerOption "cooldown" (value 60)
        "Seconds between scaling activities."
    <*> readOption "instance" "TYPE" (value M1_Medium)
        "Type of instance to provision."

instance Options Deploy where
    discover _ _ d@Deploy{..} = do
        zs <- AZ.getSuffixes dZones
        debug "Using Availability Zones '{}'" [zs]
        return $! d { dZones = zs }

    validate Deploy{..} = do
        check dZones "--zones must be specified."

        check (dMax < dMin)     "--max must be greater than or equal to --max."
        check (dDesired < dMin) "--desired must be greater than or equal to --min."
        check (dDesired > dMax) "--desired must be less than or equal to --max."

instance Naming Deploy where
    names Deploy{..} = versioned dRole dEnv dVersion

data Scale = Scale
    { sRole     :: !Text
    , sEnv      :: !Text
    , sVersion  :: !Version
    , sGrace    :: Maybe Integer
    , sMin      :: Maybe Integer
    , sMax      :: Maybe Integer
    , sDesired  :: Maybe Integer
    , sCooldown :: Maybe Integer
    }

scaleParser :: Parser Scale
scaleParser = Scale
    <$> roleOption
    <*> envOption
    <*> versionOption
    <*> optional (integerOption "grace" mempty
        "Seconds until healthchecks are activated.")
    <*> optional (integerOption "min" mempty
        "Minimum number of instances.")
    <*> optional (integerOption "max" mempty
        "Maximum number of instances.")
    <*> optional (integerOption "desired" mempty
        "Desired number of instances.")
    <*> optional (integerOption "cooldown" mempty
        "Seconds between scaling activities.")

instance Options Scale where
    validate Scale{..} = do
        check (sMin >= sMax)      "--min must be less than --max."
        check (sDesired < sMin) "--desired must be greater than or equal to --min."
        check (sDesired > sMax) "--desired must be less than or equal to --max."

instance Naming Scale where
    names Scale{..} = versioned sRole sEnv sVersion

data Cluster = Cluster
    { cRole    :: !Text
    , cEnv     :: !Text
    , cVersion :: !Version
    }

clusterParser :: Parser Cluster
clusterParser = Cluster
    <$> roleOption
    <*> envOption
    <*> versionOption

instance Options Cluster

instance Naming Cluster where
    names Cluster{..} = versioned cRole cEnv cVersion

commands :: Mod CommandFields Command
commands = group "cluster" "Auto Scaling Groups." $ mconcat
    [ command "deploy" deploy deployParser
        "Deploy a versioned cluster."
    , command "scale" scale scaleParser
        "Update the scaling information for a cluster."
    , command "promote" promote clusterParser
        "Promote a deployed cluster to serve traffic within the environment."
    , command "retire" retire clusterParser
        "Retire a specific cluster version."
    ]

deploy :: Common -> Deploy -> AWS ()
deploy c@Common{..} d@Deploy{..} = do
    j <- ASG.find d

    when (Just "Delete in progress" == join (asgStatus <$> j)) $ do
        log "Waiting for previous deletion of Auto Scaling Group {}" [appName]
        liftIO . threadDelay $ 10 * 1000000
        deploy c d

    when (isJust j) $
        throwAWS "Auto Scaling Group {} already exists." [appName]

    k <- async $ Key.create cBucket d cCerts
    p <- async $ Profile.find d
    s <- async $ Security.update (sshGroup dEnv) sshRules
    g <- async $ Security.create d
    a <- async $ AMI.find [Filter "name" [imageName]]

    wait_ k
    wait_ p <* log "Found IAM Profile {}" [profileName]
    wait_ s <* log "Found SSH Group {}" [sshGroup dEnv]
    wait_ g <* log "Found App Group {}" [groupName]

    ami <- wait a
    log "Found AMI {} named {}" [ami, imageName]

    let zones = map (AZ cRegion) dZones

    Config.create d ami dType
    ASG.create d dDomain zones dCooldown dDesired dGrace dMin dMax
  where
    Names{..} = names d

scale :: Common -> Scale -> AWS ()
scale _ s@Scale{..} = ASG.update s sCooldown sDesired sGrace sMin sMax

promote :: Common -> Cluster -> AWS ()
promote _ _ = return ()

retire :: Common -> Cluster -> AWS ()
retire _ c = ASG.delete c >> Config.delete c
