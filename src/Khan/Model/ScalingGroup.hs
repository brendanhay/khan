{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.ScalingGroup
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.ScalingGroup
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
import qualified Khan.Model.Tag              as Tag
import           Khan.Prelude                hiding (find, min, max)
import           Network.AWS.AutoScaling

findAll :: [Text] -> Source AWS AutoScalingGroup
findAll ns = do
    if null ns
        then log_ "Describing Auto Scaling Groups..."
        else say  "Describing Auto Scaling Groups: {}" [L ns]
    paginate (DescribeAutoScalingGroups (Members ns) Nothing Nothing)
        $= Conduit.map unwrap
        $= Conduit.concat
  where
    unwrap = members
        . dasgrAutoScalingGroups
        . dashrDescribeAutoScalingGroupsResult

find :: Naming a => a -> AWS (Maybe AutoScalingGroup)
find (names -> Names{..}) = findAll [appName] $$ Conduit.head

create :: Naming a
       => a
       -> Text
       -> [AvailabilityZone]
       -> Integer
       -> Integer
       -> Integer
       -> Integer
       -> Integer
       -> AWS ()
create (names -> n@Names{..}) dom zones cool desired grace min max = do
    void . send $ CreateAutoScalingGroup
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
        (Members $ map (uncurry (tag appName)) (Tag.defaults n dom))
        (Members [])
        Nothing
    say "Created Auto Scaling Group {}" [appName]
    -- Create and update level2 'name' DNS SRV record
    -- Health checks, monitoring, statistics

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
    say "Updated Auto Scaling Group {}" [appName]

delete :: Naming a => a -> AWS ()
delete (names -> Names{..}) = do
    send_ $ DeleteAutoScalingGroup appName (Just True)
    say "Delete of Auto Scaling Group {} in progress" [appName]

tag :: Text -> Text -> Text -> Tag
tag grp k v = Tag k (Just True) (Just grp) (Just "auto-scaling-group") (Just v)
