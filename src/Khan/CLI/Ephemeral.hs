{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Ephemeral
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Ephemeral (commands) where

import           Control.Concurrent      (threadDelay)
import           Data.Version
import qualified Khan.AWS.AutoScaling    as ASG
import qualified Khan.AWS.EC2            as EC2
import qualified Khan.AWS.IAM            as IAM
import           Khan.Internal
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
    discover _ d@Deploy{..} = do
        zs <- EC2.defaultZoneSuffixes dZones
        debug "Using Availability Zones '{}'" [zs]
        return $! d { dZones = zs }

    validate Deploy{..} = do
        check dRole     "--role must be specified."
        check dEnv      "--env must be specified."
        check dDomain   "--domain must be specified."
        check dVersion  "--version must be specified."
        check dGrace    "--grace must be greater than 0."
        check dMin      "--min must be greater than 0."
        check dMax      "--max must be greater than 0."
        check dDesired  "--desired must be greater than 0."
        check dCooldown "--cooldown must be greater than 0."
        check dZones    "--zones must be specified."

        check (not $ dMax >= dMin)     "--max must be greater than or equal to --max."
        check (not $ dDesired >= dMin) "--desired must be greater than or equal to --min."
        check (not $ dDesired <= dMax) "--desired must be less than or equal to --max."

        check (Within dZones "abcde")      "--zones must be within [a-e]."

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
        check sRole     "--role must be specified."
        check sEnv      "--env must be specified."
        check sVersion  "--version must be specified."
        check sGrace    "--grace must be greater than 0."
        check sMin      "--min must be greater than 0."
        check sMax      "--max must be greater than 0."
        check sDesired  "--desired must be greater than 0."
        check sCooldown "--cooldown must be greater than 0"

        check (not $ sMin < sMax)      "--min must be less than --max."
        check (not $ sDesired >= sMin) "--desired must be greater than or equal to --min."
        check (not $ sDesired <= sMax) "--desired must be less than or equal to --max."

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

instance Options Cluster where
    validate Cluster{..} = do
        check cRole    "--role must be specified."
        check cEnv     "--env must be specified."
        check cVersion "--version must be specified."

instance Naming Cluster where
    names Cluster{..} = versioned cRole cEnv cVersion

commands :: Mod CommandFields Command
commands = group "ephemeral" "Ephemeral shit."
     $ command "deploy" deploy deployParser
        "Deploy a versioned cluster."
    <> command "scale" scale scaleParser
        "Update the scaling information for a cluster."
    <> command "promote" promote clusterParser
        "Promote a deployed cluster to serve traffic within the environment."
    <> command "retire" retire clusterParser
        "Retire a specific cluster version."

deploy :: Common -> Deploy -> AWS ()
deploy c@Common{..} d@Deploy{..} = do
    j <- ASG.findGroup d

    when (Just "Delete in progress" == join (asgStatus <$> j)) $ do
        log "Waiting for previous deletion of Auto Scaling Group {}" [appName]
        liftIO . threadDelay $ 10 * 1000000
        deploy c d

    when (isJust j) $
        throwAWS "Auto Scaling Group {} already exists." [appName]

    k <- async $ EC2.createKey d
    r <- async $ IAM.findRole d
    s <- async $ EC2.updateGroup (sshGroup dEnv) sshRules
    g <- async $ EC2.createGroup d
    a <- async $ EC2.findImage [Filter "name" [imageName]]

    wait_ k
    wait_ r <* log "Found IAM Profile {}" [profileName]
    wait_ s <* log "Found SSH Group {}" [sshGroup dEnv]
    wait_ g <* log "Found App Group {}" [groupName]

    ami <- wait a
    log "Found AMI {} named {}" [ami, imageName]

    let zones = map (AZ cRegion) dZones

    ASG.createConfig d ami dType
    ASG.createGroup d dDomain zones dCooldown dDesired dGrace dMin dMax
  where
    Names{..} = names d

scale :: Common -> Scale -> AWS ()
scale _ s@Scale{..} = ASG.updateGroup s sCooldown sDesired sGrace sMin sMax

promote :: Common -> Cluster -> AWS ()
promote _ _ = return ()

retire :: Common -> Cluster -> AWS ()
retire _ c = ASG.deleteGroup c >> ASG.deleteConfig c
