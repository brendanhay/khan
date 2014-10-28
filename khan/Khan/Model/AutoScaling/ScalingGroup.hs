{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.AutoScaling.ScalingGroup
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.AutoScaling.ScalingGroup
    ( findAll
    , find
    , create
    , update
    , delete
    , tag
    ) where

import           Data.Conduit
import qualified Data.Conduit.List           as Conduit
import           Khan.Internal
import qualified Khan.Model.ELB.Types        as Balancer
import qualified Khan.Model.Tag              as Tag
import           Khan.Prelude                hiding (find, min, max)
import           Network.AWS.AutoScaling

findAll :: [Text] -> Source AWS AutoScalingGroup
findAll ns = do
    if null ns
        then log_ "Describing Auto Scaling Groups..."
        else say  "Describing Auto Scaling Groups: {}" [L ns]
    paginate (DescribeAutoScalingGroups (Members ns) Nothing Nothing)
        $= Conduit.map (members
            . dasgrAutoScalingGroups
            . dashrDescribeAutoScalingGroupsResult)
        $= Conduit.concat

find :: Naming a => a -> AWS (Maybe AutoScalingGroup)
find (names -> Names{..}) = findAll [appName] $$ Conduit.head

create :: Naming a
       => a
       -> [Balancer.BalancerName]
       -> Text
       -> [AvailabilityZone]
       -> Integer
       -> Integer
       -> Integer
       -> Integer
       -> Integer
       -> AWS ()
create (names -> n@Names{..}) bs dom zones cool desired grace min max = do
    say "Creating Auto Scaling Group {}" [appName]
    send_ CreateAutoScalingGroup
        { casgAutoScalingGroupName    = appName
        , casgLaunchConfigurationName = appName
        , casgAvailabilityZones       = Members zones
        , casgDefaultCooldown         = Just cool
        , casgDesiredCapacity         = Just desired
        , casgHealthCheckGracePeriod  = Just grace
        , casgHealthCheckType         = Just chk
        , casgLoadBalancerNames       = Members elbs
        , casgMaxSize                 = max
        , casgMinSize                 = min
        , casgPlacementGroup          = Nothing
        , casgTags                    = Members tags
        , casgTerminationPolicies     = mempty
        , casgVPCZoneIdentifier       = Nothing
        }
  where
    (chk, elbs) = if not (null bs)
                      then ("ELB", map Balancer.balancerNameText bs)
                      else ("EC2", [])

    tags = map (uncurry $ tag appName) (Tag.defaults n dom)

update :: Naming a
       => a
       -> Maybe Integer
       -> Maybe Integer
       -> Maybe Integer
       -> Maybe Integer
       -> Maybe Integer
       -> AWS ()
update (names -> n@Names{..}) cool desired grace min max = do
    AutoScalingGroup{..} <- find n >>=
        noteAWS "Auto Scaling Group {} doesn't exist." [B appName]
    say "Updating Auto Scaling Group {}" [appName]
    send_ UpdateAutoScalingGroup
        { uasgAutoScalingGroupName    = appName
        , uasgAvailabilityZones       = Members asgAvailabilityZones
        , uasgDefaultCooldown         = cool
        , uasgDesiredCapacity         = desired
        , uasgHealthCheckGracePeriod  = grace
        , uasgHealthCheckType         = Nothing
        , uasgLaunchConfigurationName = Nothing
        , uasgMaxSize                 = max
        , uasgMinSize                 = min
        , uasgPlacementGroup          = Nothing
        , uasgTerminationPolicies     = Members asgTerminationPolicies
        , uasgVPCZoneIdentifier       = Nothing
        }

delete :: Naming a => a -> AWS ()
delete (names -> Names{..}) = do
    say "Deleting of Auto Scaling Group {}" [appName]
    send_ $ DeleteAutoScalingGroup appName (Just True)

tag :: Text -> Text -> Text -> Tag
tag grp k v = Tag k (Just True) (Just grp) (Just "auto-scaling-group") (Just v)
