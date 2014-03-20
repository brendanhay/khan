{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.ELB.LoadBalancer
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.ELB.LoadBalancer
    ( findAll
    , find
    , create
    , delete
    ) where

import           Data.Conduit
import qualified Data.Conduit.List                 as Conduit
import           Khan.Internal
import qualified Khan.Model.ELB.HealthCheck        as Check
import qualified Khan.Model.ELB.LoadBalancerPolicy as Policy
import           Khan.Prelude                      hiding (find)
import           Network.AWS.ELB
import           Network.AWS.IAM                   (ServerCertificateMetadata(..))

findAll :: [Text] -> Source AWS LoadBalancerDescription
findAll bids = do
    if null bids
        then log_ "Describing Load Balancers..."
        else say  "Describing Load Balancers: {}" [L bids]
    paginateCatch (DescribeLoadBalancers (Members bids) Nothing)
        $= Conduit.mapM verify
        $= Conduit.catMaybes
        $= Conduit.concatMap (members
            . dlbrLoadBalancerDescriptions
            . dlbsDescribeLoadBalancersResult)
  where
    verify (Right x) = return (Just x)
    verify (Left  e)
        | "LoadBalancerNotFound" == elbeCode (elberError e) = return Nothing
        | otherwise = throwError (toError e)

find :: Naming a => a -> AWS (Maybe LoadBalancerDescription)
find (names -> Names{..}) = findAll [balancerName] $$ Conduit.head

create :: Naming a
       => a
       -> [AvailabilityZone]
       -> Frontend
       -> Backend
       -> ServerCertificateMetadata
       -> AWS ()
create (names -> n@Names{..}) zones fe be cert = do
    say "Creating Load Balancer {}: {} -> {}" [B balancerName, B fe, B be]
    b <- send $ CreateLoadBalancer
        { clbAvailabilityZones = Members zones
        , clbListeners         = Members [listener]
        , clbLoadBalancerName  = balancerName
        , clbScheme            = Nothing
        , clbSecurityGroups    = mempty
        , clbSubnets           = mempty
        }
    Policy.create n >> Policy.assign n fe >> Check.configure n be
    say "Load Balancer available via DNS {}"
        [clbrDNSName $ clbrCreateLoadBalancerResult b]
  where
    listener = Listener
        { lInstancePort     = port be
        , lInstanceProtocol = Just (protocolToText be)
        , lLoadBalancerPort = port fe
        , lProtocol         = protocolToText fe
        , lSSLCertificateId = Just (scmArn cert)
        }

delete :: Naming a => a -> AWS ()
delete (names -> Names{..}) = do
    say "Deleting Load Balancer {}" [balancerName]
    send_ $ DeleteLoadBalancer balancerName
