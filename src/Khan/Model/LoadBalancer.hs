{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.LoadBalancer
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.LoadBalancer
    ( find
    , create
    , delete
    ) where

import           Data.Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.Text         as Text
import           Khan.Internal
import           Khan.Prelude      hiding (find)
import           Network.AWS.ELB
import           Network.AWS.IAM   (ServerCertificateMetadata(..))

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
       -> Frontend
       -> Backend
       -> ServerCertificateMetadata
       -> [AvailabilityZone]
       -> AWS ()
create (names -> Names{..}) front back cert azs = do
    say "Creating Load Balancer {}" [appName]
    send_ $ CreateLoadBalancer
        { clbAvailabilityZones = Members azs
        , clbListeners         = Members [listener front back]
        , clbLoadBalancerName  = appName
        , clbScheme            = Nothing
        , clbSecurityGroups    = mempty
        , clbSubnets           = mempty
        }
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

-- Done as separate command?
-- 1. Create and Upload SSL Server Certificate (IAM.UploadServerCertificate)

--
-- 2. Get ARN of SSL Server Certificate (IAM.GetServerCertificate)
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

