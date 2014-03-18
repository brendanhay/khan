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
    say "Creating Load Balancer Policy {}" [appName]
    send_ $ CreateLoadBalancerPolicy
        { clbpLoadBalancerName = appName
        , clbpPolicyName       = appName
        , clbpPolicyTypeName   = "SSLNegotiationPolicyType"
        , clbpPolicyAttributes = Members [PolicyAttribute key val]
        }
  where
    key = Just "Reference-Security-Policy"
    val = Just "ELBSecurityPolicy-2014-01"

assign :: Naming a => a -> Frontend -> AWS ()
assign (names -> Names{..}) (FE _ port) = do
    say "Assigning Policy {} on Port {} to Load Balancer {}"
        [B appName, B port, B appName]
    send_ $ SetLoadBalancerPoliciesOfListener
        { slbpolLoadBalancerName = appName
        , slbpolLoadBalancerPort = port
        , slbpolPolicyNames      = Members [appName]
        }
