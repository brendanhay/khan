{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.ELB.LoadBalancerPolicy
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.ELB.LoadBalancerPolicy
    ( create
    , assign
    , sslPolicy
    , proxyProtocolPolicy
    ) where

import Khan.Model.ELB.Types
import Khan.Prelude
import Network.AWS.ELB


create :: CreateLoadBalancerPolicy -> AWS PolicyName
create rq = do
    say "Creating Load Balancer Policy {}" [clbpPolicyName rq]
    send_ rq
    return $ policyNameFromCreateRequest rq

sslPolicy :: BalancerName -> CreateLoadBalancerPolicy
sslPolicy balancer = CreateLoadBalancerPolicy
    { clbpLoadBalancerName = balancerNameText balancer
    , clbpPolicyName       = policyNameText $ mkPolicyName balancer typ
    , clbpPolicyTypeName   = policyTypeText typ
    , clbpPolicyAttributes = Members [PolicyAttribute key val]
    }
  where
    typ = PolicyType "SSLNegotiationPolicyType"
    key = Just "Reference-Security-Policy"
    val = Just "ELBSecurityPolicy-2014-01"


proxyProtocolPolicy :: BalancerName -> CreateLoadBalancerPolicy
proxyProtocolPolicy balancer = CreateLoadBalancerPolicy
    { clbpLoadBalancerName = balancerNameText balancer
    , clbpPolicyName       = policyNameText $ mkPolicyName balancer typ
    , clbpPolicyTypeName   = policyTypeText typ
    , clbpPolicyAttributes = Members [PolicyAttribute key val]
    }
  where
    typ = PolicyType "ProxyProtocolPolicyType"
    key = Just "ProxyProtocol"
    val = Just "true"


assign :: PolicyName -> BalancerName -> PortNumber -> AWS ()
assign policy balancer port = do
    say "Assigning Policy {} on Port {} of Balancer {}" [B policy, B $ toInteger port, B balancer]
    send_ SetLoadBalancerPoliciesOfListener
        { slbpolLoadBalancerName = balancerNameText balancer
        , slbpolLoadBalancerPort = toInteger port
        , slbpolPolicyNames      = Members [policyNameText policy]
        }
