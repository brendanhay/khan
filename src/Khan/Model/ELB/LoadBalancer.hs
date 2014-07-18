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
    , fqdn
    ) where

import           Data.Conduit
import qualified Data.Conduit.List                 as Conduit
import qualified Data.Text                         as Text
import           Khan.Internal
import qualified Khan.Model.ELB.HealthCheck        as Check
import qualified Khan.Model.ELB.LoadBalancerPolicy as Policy
import           Khan.Model.ELB.Types
import           Khan.Prelude                      hiding (find)
import           Network.AWS.ELB
import           Network.AWS.IAM                   (ServerCertificateMetadata(..))

findAll :: [Name] -> Source AWS LoadBalancerDescription
findAll nms = do
    if null nms
        then log_ "Describing Load Balancers..."
        else say  "Describing Load Balancers: {}" [L nms]
    paginateCatch (DescribeLoadBalancers (Members (map nameText nms)) Nothing)
        $= Conduit.mapM verify
        $= Conduit.catMaybes
        $= Conduit.concatMap (members
            . dlbrLoadBalancerDescriptions
            . dlbsDescribeLoadBalancersResult)
  where
    verify (Right b) = return (Just b)
    verify (Left  e)
        | "LoadBalancerNotFound" == elbeCode (elberError e) = return Nothing
        | otherwise = throwError (toError e)

find :: Name -> AWS (Maybe LoadBalancerDescription)
find name = findAll [name] $$ Conduit.head

create :: Naming a
       => a
       -> [AvailabilityZone]
       -> Mapping
       -> ServerCertificateMetadata
       -> AWS ()
create (names -> n@Names{..}) zones m@(Mapping fe be) cert = do
    let name = mkName n m
    say "Creating Load Balancer {}: {} -> {}" [B name, B fe, B be]
    blc <- send CreateLoadBalancer
        { clbAvailabilityZones = Members zones
        , clbListeners         = Members [listener]
        , clbLoadBalancerName  = nameText name
        , clbScheme            = Nothing
        , clbSecurityGroups    = mempty
        , clbSubnets           = mempty
        }
    Policy.create (Policy.sslPolicy name)
    Policy.assign name (frontendPort fe)
    case backendProtocol be of
        TCP -> Policy.create (Policy.proxyProtocolPolicy name)
            >> Policy.assign name (backendPort be)
        _   -> return ()
    Check.configure name (backendHealthCheck be)
    say "Load Balancer available via DNS {}"
        [clbrDNSName $ clbrCreateLoadBalancerResult blc]
  where
    listener = Listener
        { lInstancePort     = toInteger (backendPort be)
        , lInstanceProtocol = Just (protocolText (backendProtocol be))
        , lLoadBalancerPort = toInteger (frontendPort fe)
        , lProtocol         = protocolText (frontendProtocol fe)
        , lSSLCertificateId = Just (scmArn cert)
        }

delete :: Name -> AWS ()
delete name = do
    say "Deleting Load Balancer {}" [name]
    send_ $ DeleteLoadBalancer (nameText name)

fqdn :: Naming a => a -> Text -> LoadBalancerDescription -> AWS Text
-- Note: As per 'create' we only use a single listener per ELB.
fqdn (names -> Names{..}) dom LoadBalancerDescription{..} =
    let l = ldListener =<< listToMaybe (members lbdListenerDescriptions)
    in maybe missing (return . mkDNS) (lProtocol <$> l)
  where
    mkDNS p = dnsName <> "-" <> Text.toLower p <> "." <> dom
    missing = throwAWS "Load Balancer {} has no listener." [lbdLoadBalancerName]
