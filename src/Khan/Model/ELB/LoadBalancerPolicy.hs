{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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

import Khan.Model.ELB.Types (Name, PortNumber, nameText)
import Khan.Prelude
import Network.AWS.ELB

create :: CreateLoadBalancerPolicy -> AWS ()
create p = do
    say "Creating Load Balancer Policy {}" [clbpPolicyName p]
    send_ p

sslPolicy :: Name -> CreateLoadBalancerPolicy
sslPolicy name = CreateLoadBalancerPolicy
    { clbpLoadBalancerName = nameText name
    , clbpPolicyName       = nameText name
    , clbpPolicyTypeName   = "SSLNegotiationPolicyType"
    , clbpPolicyAttributes = Members [PolicyAttribute key val]
    }
  where
    key = Just "Reference-Security-Policy"
    val = Just "ELBSecurityPolicy-2014-01"

proxyProtocolPolicy :: Name -> CreateLoadBalancerPolicy
proxyProtocolPolicy name = CreateLoadBalancerPolicy
    { clbpLoadBalancerName = nameText name
    , clbpPolicyName       = nameText name
    , clbpPolicyTypeName   = "ProxyProtocolPolicyType"
    , clbpPolicyAttributes = Members [PolicyAttribute key val]
    }
  where
    key = Just "ProxyProtocol"
    val = Just "true"

assign :: Name -> PortNumber -> AWS ()
assign name port = do
    say "Assigning Policy {} on Port {}" [B name, B $ toInteger port]
    send_ SetLoadBalancerPoliciesOfListener
        { slbpolLoadBalancerName = nameText name
        , slbpolLoadBalancerPort = toInteger port
        , slbpolPolicyNames      = Members [nameText name]
        }
