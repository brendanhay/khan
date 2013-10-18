{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.AWS.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.AWS.AutoScaling where

import Khan.Internal
import Khan.Prelude            hiding (min, max)
import Network.AWS
import Network.AWS.AutoScaling hiding (Filter)
import Network.AWS.Internal

createConfig :: Naming a => a -> Text -> InstanceType -> AWS ()
createConfig (names -> Names{..}) ami typ = do
    c <- sendCatch $ CreateLaunchConfiguration
        (Members [])
        Nothing
        (Just profileName)              -- Instance Profile
        ami                             -- Image Id
        Nothing
        typ                             -- Instance Type
        Nothing
        (Just keyName)                  -- Key Pair Name
        appName                         -- Launch Configuration Name
        Nothing
        (Members [groupName, sshGroup envName]) -- Security Groups
        Nothing
        Nothing                         -- User Data
    verifyAS "AlreadyExists" c
    log "Created Launch Configuration {}" [appName]

deleteConfig :: Naming a => a -> AWS ()
deleteConfig (names -> Names{..}) = do
    send_ $ DeleteLaunchConfiguration appName
    log "Deleted Launch Configuration {}" [appName]

findGroup :: Naming a => a -> AWS (Maybe AutoScalingGroup)
findGroup (names -> Names{..}) = fmap
    (listToMaybe . members . dasgrAutoScalingGroups . dashrDescribeAutoScalingGroupsResult)
    (send $ DescribeAutoScalingGroups (Members [appName]) Nothing Nothing)

createGroup :: Naming a
            => a
            -> Text
            -> [AvailabilityZone]
            -> Integer
            -> Integer
            -> Integer
            -> Integer
            -> Integer
            -> AWS ()
createGroup (names -> n@Names{..}) dom zones cool desired grace min max = do
    send_ $ CreateAutoScalingGroup
        appName                        -- Name
        (Members zones)                -- Zones
        (Just cool)                    -- Default Cooldown
        (Just desired)                 -- Desired Capacity
        (Just grace)                   -- Grace Period
        (Just "EC2")                   -- Health Check Type: EC2 | ELB
        appName                        -- Launch Configuration Name
        (Members [])
        max
        min
        Nothing
        (Members . map (uncurry tag) $ defaultTags n dom) -- Tags
        (Members [])
        Nothing
    log "Created Auto Scaling Group {}" [appName]
    -- Create and update level2 'name' DNS SRV record
    -- Health checks, monitoring, statistics
  where
    tag k v = Tag k
       (Just True)
       (Just appName)
       (Just "auto-scaling-group")
       (Just v)

updateGroup :: Naming a
            => a
            -> Maybe Integer
            -> Maybe Integer
            -> Maybe Integer
            -> Maybe Integer
            -> Maybe Integer
            -> AWS ()
updateGroup (names -> n@Names{..}) cool desired grace min max = do
    AutoScalingGroup{..} <- findGroup n >>=
        noteAWS "Auto Scaling Group %s doesn't exist." [appName]
    send_ $ UpdateAutoScalingGroup
        appName
        (Members asgAvailabilityZones)
        cool
        desired
        grace
        Nothing
        Nothing
        max
        min
        Nothing
        (Members asgTerminationPolicies)
        Nothing
    log "Updated Auto Scaling Group {}" [appName]

deleteGroup :: Naming a => a -> AWS ()
deleteGroup (names -> Names{..}) = do
    send_ $ DeleteAutoScalingGroup appName (Just True)
    log "Delete of Auto Scaling Group {} in progress" [appName]
