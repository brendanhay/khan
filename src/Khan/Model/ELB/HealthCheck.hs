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

import Data.Text.Lazy             (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Khan.Internal
import Khan.Prelude
import Network.AWS.ELB

configure :: Naming a => a -> Frontend -> AWS ()
configure (names -> Names{..}) fe@(FE _ port) = do
    say "Configuring Health Check on {} for Balancer {}" [B fe, B appName]
    send_ $ ConfigureHealthCheck chk appName
  where
    target = toStrict . toLazyText $ "http:" <> decimal port <> "/i/status"

    chk = HealthCheck
        { hcHealthyThreshold   = 3  -- Required healthy responses.
        , hcUnhealthyThreshold = 2  -- Required unhealthy responses.
        , hcInterval           = 10 -- Seconds between checks.
        , hcTarget             = target
        , hcTimeout            = 3  -- Seconds.
        }
