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
    ) where

import Khan.Internal
import Khan.Prelude
import Network.AWS.ELB

create :: Naming a => a -> AWS ()
create (names -> Names{..}) = do
    say "Creating Load Balancer Policy {}" [balancerName]
    send_ CreateLoadBalancerPolicy
        { clbpLoadBalancerName = balancerName
        , clbpPolicyName       = balancerName
        , clbpPolicyTypeName   = "SSLNegotiationPolicyType"
        , clbpPolicyAttributes = Members [PolicyAttribute key val]
        }
  where
    key = Just "Reference-Security-Policy"
    val = Just "ELBSecurityPolicy-2014-01"

assign :: Naming a => a -> Frontend -> AWS ()
assign (names -> Names{..}) fe = do
    say "Assigning Policy {} on Port {}" [B balancerName, B $ port fe]
    send_ SetLoadBalancerPoliciesOfListener
        { slbpolLoadBalancerName = balancerName
        , slbpolLoadBalancerPort = port fe
        , slbpolPolicyNames      = Members [balancerName]
        }
