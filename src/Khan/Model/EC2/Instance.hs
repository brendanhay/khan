{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Model.EC2.Instance
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.EC2.Instance
    ( findAll
    , run
    , wait
    ) where

import           Control.Arrow                ((***))
import           Control.Monad
import           Data.List                    (partition)
import           Khan.Internal
import qualified Khan.Model.EC2.SecurityGroup as Security
import           Khan.Prelude                 hiding (min, max)
import           Network.AWS.EC2              hiding (Instance, wait)

findAll :: [Text] -> [Filter] -> AWS [RunningInstancesItemType]
findAll ids = fmap (concatMap ritInstancesSet . dirReservationSet) .
    send . DescribeInstances ids

run :: Naming a
    => a
    -> Text
    -> InstanceType
    -> AvailabilityZone
    -> Integer
    -> Integer
    -> Bool
    -> AWS [RunningInstancesItemType]
run (names -> n@Names{..}) image typ az min max opt = do
    say "Running [{}..{}] Instances in {}" [B min, B max, B az]
    gs <- Security.defaults n
    fmap rirInstancesSet . send $ RunInstances
        { riImageId                           = image
        , riMinCount                          = min
        , riMaxCount                          = max
        , riKeyName                           = Just keyName
        , riSecurityGroupId                   = []
        , riSecurityGroup                     = gs
        , riUserData                          = Nothing
        , riInstanceType                      = Just typ
        , riPlacement                         = Just place
        , rjKernelId                          = Nothing
        , rjRamdiskId                         = Nothing
        , rjBlockDeviceMapping                = []
        , rjMonitoring                        = Just monitor
        , rjSubnetId                          = Nothing
        , rjDisableApiTermination             = Nothing
        , rjInstanceInitiatedShutdownBehavior = Nothing
        , rjPrivateIpAddress                  = Nothing
        , rjClientToken                       = Nothing
        , rjNetworkInterface                  = []
        , rjIamInstanceProfile                = [profile]
        , rjEbsOptimized                      = Just opt
        }
  where
    place   = PlacementType (Just az) Nothing Nothing
    monitor = MonitoringInstanceType True
    profile = IamInstanceProfileRequestType Nothing (Just profileName)

wait :: [Text] -> AWS ()
wait []  = log_ "All instances running."
wait ids = do
    xs <- findAll ids []
    let (ps, rs) = join (***) (map riitInstanceId) $ pending xs
    unless (null rs) $
        say "Instances marked as running: {}" [rs]
    unless (null ps) $ do
        say "Instances still pending: {}" [ps]
        log_ "Waiting..."
        delaySeconds 30
    wait ps
  where
    pending = partition (("pending" ==) . istName . riitInstanceState)
