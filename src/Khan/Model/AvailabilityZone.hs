{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Khan.Model.AvailabilityZone
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.AvailabilityZone
    ( findAll
    , getSuffixes
    ) where

import qualified Data.Text       as Text
import           Khan.Internal
import           Khan.Prelude    hiding (min, max)
import           Network.AWS.EC2 hiding (Instance)

findAll :: AWS [AvailabilityZoneItemType]
findAll = do
    reg <- Text.pack . show <$> getRegion
    debug_ "Finding Availability Zones in current region"
    fmap dazrAvailabilityZoneInfo . send $
        DescribeAvailabilityZones []
            [ Filter "region-name" [reg]
            , Filter "state" ["available"]
            ]

getSuffixes :: String -> AWS String
getSuffixes sufs
    | invalid sufs = map (azSuffix . azitZoneName) <$> findAll
    | otherwise    = return sufs
