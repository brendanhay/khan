{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.ELB.HealthCheck
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.ELB.HealthCheck
    ( configure
    ) where

import Khan.Model.ELB.Types
import Khan.Prelude
import Network.AWS.ELB

configure :: Name -> Text -> AWS ()
configure name url = do
    say "Configuring Health Check on {} for Balancer {}" [B url, B name]
    send_ $ ConfigureHealthCheck chk (nameText name)
  where
    chk = HealthCheck
        { hcHealthyThreshold   = 3  -- Required healthy responses.
        , hcUnhealthyThreshold = 2  -- Required unhealthy responses.
        , hcInterval           = 10 -- Seconds between checks.
        , hcTarget             = url
        , hcTimeout            = 3  -- Seconds.
        }
