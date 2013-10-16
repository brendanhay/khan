{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.CLI.Application
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Application (cli) where

import           Control.Concurrent      (threadDelay)
import qualified Khan.AWS.AutoScaling    as ASG
import qualified Khan.AWS.EC2            as EC2
import qualified Khan.AWS.IAM            as IAM
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.AutoScaling hiding (Filter)
import           Network.AWS.EC2

defineOptions "Deploy" $ do
    textOption "dName" "name" ""
        "Name of the application."

    textOption "dEnv" "env" defaultEnv
        "Environment to deploy the application into."

    textOption "dDomain" "domain" ""
        "Instance's DNS domain."

-- FIXME: Add port(s) option, and apply to tags

    versionOption "dVersion" "version" defaultVersion
        "Version of the application."

    stringOption "dZones" "zones" ""
         "Availability zones suffixes to provision into."

    integerOption "dGrace" "grace" 20
        "Seconds until healthchecks are activated."

    integerOption "dMin" "min" 1
        "Minimum number of instances."

    integerOption "dMax" "max" 1
        "Maximum number of instances."

    integerOption "dDesired" "desired" 1
        "Desired number of instances."

    integerOption "dCooldown" "cooldown" 60
        "Seconds between scaling activities."

    instanceTypeOption "dType" "instance" M1_Medium
        "Type of instance to provision."

    rulesOption "dRules" "rules"
        "IP permission specifications."

deriving instance Show Deploy

instance Discover Deploy where
    discover d = do
        zs <- map (azSuffix . azitZoneName) <$> EC2.findCurrentZones
        logInfo "Using Availability Zones '{}'" [zs]
        return $! d { dZones = zs }

instance Validate Deploy where
    validate Deploy{..} = do
        check dName     "--name must be specified."
        check dEnv      "--env must be specified."
        check dDomain   "--domain must be specified."
        check dGrace    "--grace must be greater than 0."
        check dMin      "--min must be greater than 0."
        check dMax      "--max must be greater than 0."
        check dDesired  "--desired must be greater than 0."
        check dCooldown "--cooldown must be greater than 0."
        check dZones    "--zones must be specified."

        check (not $ dMax >= dMin)     "--max must be greater than or equal to --max."
        check (not $ dDesired >= dMin) "--desired must be greater than or equal to --min."
        check (not $ dDesired <= dMax) "--desired must be less than or equal to --max."

        check (Within dZones "abc")        "--zones must be within [a-e]."
        check (defaultVersion == dVersion) "--version must be specified."

instance Naming Deploy where
    names Deploy{..} = versioned dName dEnv dVersion

defineOptions "Scale" $ do
    textOption "sName" "name" ""
        "Name of the application."

    textOption "sEnv" "env" defaultEnv
        "Environment to deploy the application into."

    versionOption "sVersion" "version" defaultVersion
        "Version of the application."

    maybeIntegerOption "sGrace" "grace" 20
        "Seconds until healthchecks are activated."

    maybeIntegerOption "sMin" "min" 1
        "Minimum number of instances."

    maybeIntegerOption "sMax" "max" 20
        "Maximum number of instances."

    maybeIntegerOption "sDesired" "desired" 2
        "Desired number of instances."

    maybeIntegerOption "sCooldown" "cooldown" 60
        "Seconds between scaling activities."

deriving instance Show Scale

instance Discover Scale

instance Validate Scale where
    validate Scale{..} = do
        check sName     "--name must be specified."
        check sEnv      "--env must be specified."
        check sGrace    "--grace must be greater than 0."
        check sMin      "--min must be greater than 0."
        check sMax      "--max must be greater than 0."
        check sDesired  "--desired must be greater than 0."
        check sCooldown "--cooldown must be greater than 0"

        check (not $ sMin < sMax)      "--min must be less than --max."
        check (not $ sDesired >= sMin) "--desired must be greater than or equal to --min."
        check (not $ sDesired <= sMax) "--desired must be less than or equal to --max."

        check (defaultVersion == sVersion) "--version must be specified."

instance Naming Scale where
    names Scale{..} = versioned sName sEnv sVersion

defineOptions "Cluster" $ do
    textOption "cName" "name" ""
        "Name of the application."

    textOption "cEnv" "env" defaultEnv
        "Environment to deploy the application into."

    versionOption "cVersion" "version" defaultVersion
        "Version of the application."

deriving instance Show Cluster

instance Discover Cluster

instance Validate Cluster where
    validate Cluster{..} = do
        check cName "--name must be specified."
        check cEnv  "--env must be specified."
        check (defaultVersion == cVersion) "--version must be specified."

instance Naming Cluster where
    names Cluster{..} = versioned cName cEnv cVersion

cli :: Command
cli = Command "app" "Manage Applications."
    [ subCommand "deploy"  deploy
    , subCommand "scale"   scale
    , subCommand "retire"  retire
    , subCommand "promote" promote
    , subCommand "info"    info
    ]

deploy :: Deploy -> AWS ()
deploy d@Deploy{..} = do
    j <- ASG.findGroup d

    when (Just "Delete in progress" == join (asgStatus <$> j)) $ do
        logInfo "Waiting for previous deletion of Auto Scaling Group {}" [appName]
        liftIO . threadDelay $ 10 * 1000000
        deploy d

    when (isJust j) $
        throwErrorF "Auto Scaling Group {} already exists." [appName]

    k <- async $ EC2.createKey d
    r <- async $ IAM.findRole d
    s <- async $ EC2.updateGroup (sshGroup dEnv) sshRules
    g <- async $ EC2.updateGroup d dRules
    a <- async $ EC2.findImage [imageName]

    wait_ k
    wait_ r <* logInfo "Found IAM Profile {}" [profileName]
    wait_ s <* logInfo "Found SSH Group {}" [sshGroup dEnv]
    wait_ g <* logInfo "Found App Group {}" [groupName]

    ami <- wait a
    logInfo "Found AMI {} named {}" [ami, imageName]

    reg <- currentRegion

    let zones = map (AZ reg) dZones

    ASG.createConfig d ami dType
    ASG.createGroup d dDomain zones dCooldown dDesired dGrace dMin dMax
  where
    Names{..} = names d

scale :: Scale -> AWS ()
scale s@Scale{..} = ASG.updateGroup s sCooldown sDesired sGrace sMin sMax

retire :: Cluster -> AWS ()
retire c@Cluster{..} = ASG.deleteGroup c >> ASG.deleteConfig c

promote :: Cluster -> AWS ()
promote Cluster{..} = return ()

info :: Cluster -> AWS ()
info Cluster{..} = return ()
