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

import           Data.Conduit
import qualified Data.Conduit.List as Conduit
import qualified Data.Text         as Text
import           Khan.Internal
import           Khan.Prelude      hiding (find)
import           Network.AWS.ELB
import           Network.AWS.IAM   (ServerCertificateMetadata(..))

-- Create a naming rule for the port/domain pair?

create dom pas = undefined --do
    -- send $ CreateLoadBalancerPolicy
    --     { clbpLoadBalancerName = dom
    --     , clbpPolicyAttributes = Members [PolicyAttributes (Just "Reference-Security-Policy") (Just "ELBSecurityPolicy-2014-01")]
    --     , clbpPolicyName       = "ReferenceSSLNegotiationPolicy"
    --     , clbpPolicyTypeName   = "SSLNegotiationPolicyType"
    --     }

assign dom port = undefined -- do
    -- send $ SetLoadBalancerPoliciesOfListener
    --     { slbpolLoadBalancerName = dom
    --     , slbpolLoadBalancerPort = port
    --     , slbpolPolicyNames      = Members []
    --       -- ^ List of policies to be associated with the listener. Currently
    --       -- this list can have at most one policy. If the list is empty, the
    --       -- current policy is removed from the listener.
    --     }
