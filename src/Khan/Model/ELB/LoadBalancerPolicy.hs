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

import Khan.Model.ELB.Types (Name, Port, nameText)
import Khan.Prelude
import Network.AWS.ELB

create :: Name -> AWS ()
create name = do
    say "Creating Load Balancer Policy {}" [name]
    send_ CreateLoadBalancerPolicy
        { clbpLoadBalancerName = nameText name
        , clbpPolicyName       = nameText name
        , clbpPolicyTypeName   = "SSLNegotiationPolicyType"
        , clbpPolicyAttributes = Members [PolicyAttribute key val]
        }
  where
    key = Just "Reference-Security-Policy"
    val = Just "ELBSecurityPolicy-2014-01"

assign :: Name -> Port -> AWS ()
assign name port = do
    say "Assigning Policy {} on Port {}" [B name, B port]
    send_ SetLoadBalancerPoliciesOfListener
        { slbpolLoadBalancerName = nameText name
        , slbpolLoadBalancerPort = port
        , slbpolPolicyNames      = Members [nameText name]
        }
