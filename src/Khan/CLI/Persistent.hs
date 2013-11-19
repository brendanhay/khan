{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Persistent
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Persistent (commands) where

import           Khan.Internal
import qualified Khan.Model.AvailabilityZone as AZ
import qualified Khan.Model.Image            as AMI
import qualified Khan.Model.Instance         as Instance
import qualified Khan.Model.Key              as Key
import qualified Khan.Model.LaunchConfig     as Config
import qualified Khan.Model.Profile          as Profile
import qualified Khan.Model.RecordSet        as DNS
import qualified Khan.Model.ScalingGroup     as ASG
import qualified Khan.Model.SecurityGroup    as Security
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2

data Launch = Launch
    { lRole      :: !Text
    , lEnv       :: !Text
    , lDomain    :: !Text
    , lImage     :: Maybe Text
    , lNum       :: !Integer
    , lGroups    :: [Text]
    , lType      :: !InstanceType
    , lOptimised :: !Bool
    , lZones     :: !String
    }

launchParser :: Parser Launch
launchParser = Launch
    <$> roleOption
    <*> envOption
    <*> textOption "domain" (short 'd')
        "Instance's DNS domain."
    <*> optional (textOption "image" (value "")
        "Id of the image/ami.")
    <*> integerOption "num" (short 'n' <> value 1)
        "Number of instances to launch."
    <*> many (textOption "group" mempty
        "Security groups. (discovered)")
    <*> readOption "type" "TYPE" (value M1_Small)
        "Instance's type."
    <*> switchOption "optimised" False
        "EBS optimisation."
    <*> stringOption "zones" (value "")
         "Availability zones suffixes to provision into (psuedo-random)."

    -- Block Device Mappings
    -- Monitoring
    -- Disable Api Termination
    -- Instance Shutdown Behavior
    -- Client Token
    -- Network Interfaces

instance Options Launch where
    discover _ l@Launch{..} = do
        zs <- AZ.getSuffixes lZones
        debug "Using Availability Zones '{}'" [zs]
        return $! l { lZones = zs }

    validate Launch{..} =
        check lZones "--zones must be specified."

instance Naming Launch where
    names Launch{..} = unversioned lRole lEnv

data Host = Host
    { hRole  :: !Text
    , hEnv   :: !Text
    , hKey   :: !FilePath
    , hHosts :: [Text]
    }

hostParser :: Parser Host
hostParser = Host
    <$> roleOption
    <*> envOption
    <*> keyOption
    <*> many (textOption "host" mempty
        "Hosts to run on.")

instance Options Host where
    discover _ h@Host{..}
        | not $ invalid hKey = return h
        | otherwise = do
            f <- keyPath $ names h
            return $! h { hKey = f }

    validate Host{..} = do
        if invalid hHosts
            then check hRole "--role must be specified." >>
                 check hEnv  "--env must be specified."
            else check hHosts "--host must be specified at least once."
        checkPath hKey " specified by --key must exist."

instance Naming Host where
    names Host{..} = unversioned hRole hEnv

commands :: Mod CommandFields Command
commands = mconcat
    [ command "launch" launch launchParser
        "Launch 1..n instances."
    , command "terminate" terminate hostParser
        "Terminate a single instance."
    ]

launch :: Common -> Launch -> AWS ()
launch Common{..} l@Launch{..} = do
    a <- async . AMI.find . (:[]) $ maybe (Filter "name" [imageName])
        (Filter "image-id" . (:[])) lImage
    i <- async $ Profile.find l

    ami <- wait a
    log "Using Image {}" [ami]

    wait_ i <* log "Found IAM Profile {}" [profileName]

    k <- async $ Key.create l
    s <- async $ Security.update (sshGroup lEnv) sshRules
    g <- async $ Security.create l

    wait_ k <* log "Found KeyPair {}" [keyName]
    wait_ s <* log "Found SSH Group {}" [sshGroup lEnv]
    wait_ g <* log "Found Role Group {}" [groupName]

    az  <- shuffle lZones
    ms1 <- Instance.run l ami lType (AZ cRegion az) lNum lNum lOptimised

    let ids = map riitInstanceId ms1

    Instance.tag l lDomain ids
    Instance.wait ids
  where
    Names{..} = names l

terminate :: Common -> Host -> AWS ()
terminate _ _ = return ()
