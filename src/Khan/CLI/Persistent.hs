{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

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

import qualified Khan.AWS.EC2              as EC2
import qualified Khan.AWS.IAM              as IAM
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2

defineOptions "Launch" $ do
    textOption "lRole" "role" ""
        "Instance's role. (required)"

    textOption "lDomain" "domain" ""
        "Instance's DNS domain. (required)"

    textOption "lEnv" "env" defaultEnv
        "Instance's environment."

    maybeTextOption "lImage" "image" ""
        "Id of the image/ami. (discovered)"

    integerOption "lMin" "min" 1
        "Minimum number of instances to launch."

    integerOption "lMax" "max" 1
        "Maximum number of instances to launch."

    textsOption "lGroups" "groups" []
        "Security groups. (discovered)"

    instanceTypeOption "lType" "type" M1_Small
        "Instance's type."

    boolOption "lOptimised" "optimised" False
        "EBS optimisation."

    rulesOption "lRules" "rules"
        "IP permission specifications."

    stringOption "lZones" "zones" "abc"
         "Availability zones suffixes to provision into (psuedo-random)."

    textOption "lUser" "user" "ubuntu"
        "SSH user."

    intOption "lTimeout" "timeout" 60
        "SSH timeout."

    pathOption "lKeys" "keys" ""
        "Directory for private keys. (default: /etc/khan or <bin>/config)"

    -- Block Device Mappings
    -- Monitoring
    -- Disable Api Termination
    -- Instance Shutdown Behavior
    -- Client Token
    -- Network Interfaces

deriving instance Show Launch

instance Discover Launch where
    discover _ l@Launch{..} = do
        ks <- defaultPath defaultKeyDir lKeys
        log "Using Key Path {}" [ks]
        zs <- EC2.defaultZoneSuffixes lZones
        log "Using Availability Zones '{}'" [zs]
        return $! l { lKeys = ks, lZones = zs }

instance Validate Launch where
    validate Launch{..} = do
        check lRole   "--role must be specified."
        check lEnv    "--env must be specified."
        check lDomain "--domain must be specified."
        check lMin    "--min must be greater than 0."
        check lMax    "--max must be greater than 0."
        check lZones  "--zones must be specified."
        check lKeys   "--keys must be specified."

        check (not $ lMin <= lMax)    "--min must be less than or equal to --max."
        check (Within lZones "abcde") "--zones must be within [a-e]."

instance Naming Launch where
    names Launch{..} = unversioned lRole lEnv

defineOptions "Host" $ do
    textOption "hRole" "role" ""
        "Instance's role."

    textOption "hEnv" "env" defaultEnv
        "Instance's environment."

    textsOption "hHosts" "hosts" []
        "Hosts to run on."

    pathOption "hKeys" "keys" ""
        "Directory for private keys."

deriving instance Show Host

instance Discover Host where
    discover _ h@Host{..}= do
        ks <- defaultPath defaultKeyDir hKeys
        log "Using Key Path {}" [ks]
        return $! h { hKeys = ks }

instance Validate Host where
    validate Host{..} = do
        if invalid hHosts
            then check hRole "--role must be specified." >>
                 check hEnv  "--env must be specified."
            else check hHosts "--hosts must be specified."
        check hKeys "--keys must be specified."

commands :: [Command]
commands =
    [ command launch "launch" "Launch 1..n instances."
        "Some text about launching."
    , command terminate "terminate" "Terminate a single instance."
        "Some text about terminating."
    ]

launch :: Launch -> AWS ()
launch l@Launch{..} = do
    ami <- EC2.findImage . (:[]) $ maybe (Filter "name" [imageName])
        (Filter "image-id" . (:[])) lImage

    log "Using Image {}" [ami]

    i <- async $ IAM.findRole l
    k <- async $ EC2.createKey l lKeys
    s <- async $ EC2.updateGroup (sshGroup lEnv) sshRules
    g <- async $ EC2.updateGroup l lRules

    wait_ i <* log "Found IAM Profile {}" [profileName]
    wait_ k <* log "Found KeyPair {}" [keyName]
    wait_ s <* log "Found SSH Group {}" [sshGroup lEnv]
    wait_ g <* log "Found Role Group {}" [groupName]

    az  <- shuffle lZones
    reg <- getRegion
    ms1 <- EC2.runInstances l ami lType (AZ reg az) lMin lMax lOptimised

    let ids = map riitInstanceId ms1

    EC2.tagInstances l lDomain ids
    EC2.waitForInstances ids
  where
    Names{..} = names l

terminate :: Host -> AWS ()
terminate Host{..} = return ()
