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
    ( find
    , create
    , delete
    ) where

import           Data.Conduit
import qualified Data.Conduit.List                 as Conduit
import qualified Data.Text                         as Text
import           Khan.Internal
import qualified Khan.Model.ELB.LoadBalancerPolicy as Policy
import           Khan.Prelude                      hiding (find)
import           Network.AWS.ELB
import           Network.AWS.IAM                   (ServerCertificateMetadata(..))

data Protocol
    = HTTP
    | HTTPS
    | TCP
    | SSL
      deriving (Show)

protoToText :: Protocol -> Text
protoToText = Text.toLower . Text.pack . show

data Frontend = FE !Protocol !Integer
data Backend  = BE !Protocol !Integer

find :: Naming a => a -> AWS (Maybe LoadBalancerDescription)
find (names -> Names{..}) = findAll [appName] $$ Conduit.head

findAll :: [Text] -> Source AWS LoadBalancerDescription
findAll bids = do
    say "Searching for Load Balancers matching {}" [bids]
    paginate (DescribeLoadBalancers (Members bids) Nothing)
        $= Conduit.concatMap (members
            . dlbrLoadBalancerDescriptions
            . dlbsDescribeLoadBalancersResult)

create :: Naming a
       => a
       -> [AvailabilityZone]
       -> Frontend
       -> Backend
       -> ServerCertificateMetadata
       -> AWS ()
create (names -> Names{..}) zones front back cert = do
    say "Creating Load Balancer {}" [appName]
    elb <- send $ CreateLoadBalancer
        { clbAvailabilityZones = Members zones
        , clbListeners         = Members [listener front back]
        , clbLoadBalancerName  = appName
        , clbScheme            = Nothing
        , clbSecurityGroups    = mempty
        , clbSubnets           = mempty
        }
    say "Created Load Balancer DNS {}"
        [clbrDNSName $ clbrCreateLoadBalancerResult elb]
  where
    listener (FE fs fp) (BE ts tp) = Listener
        { lInstancePort     = fp
        , lInstanceProtocol = Just (protoToText fs)
        , lLoadBalancerPort = tp
        , lProtocol         = protoToText ts
        , lSSLCertificateId = Just (scmArn cert)
        }

delete :: Naming a => a -> AWS ()
delete (names -> Names{..}) = do
    say "Deleting Load Balancer {}" [appName]
    send_ $ DeleteLoadBalancer appName

--
-- 3. ELB.CreateLoadBalancer (returns DNS name)
-- 5. Configure SSL Security Policy
-- 6. Configure Backend Server Auth
-- 7. Configure Health Checks
-- 8. skip this step - Register Backend Instances

-- 4. R53.CreateRecordSet - should be last?
--    Shouldn't happen at all, only via promote?


-- Deploy:
-- 1. create the ELB
-- 2. create the ASG and attach the ELB

