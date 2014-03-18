{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.AutoScaling.LaunchConfig
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.AutoScaling.LaunchConfig
    ( create
    , delete
    ) where

import Control.Monad
import Khan.Internal
import Khan.Prelude            hiding (min, max)
import Network.AWS.AutoScaling hiding (Filter)

create :: Naming a => a -> Text -> InstanceType -> AWS ()
create (names -> Names{..}) ami typ = do
    c <- sendCatch $ CreateLaunchConfiguration
        { clcBlockDeviceMappings     = mempty
        , clcEbsOptimized            = Nothing
        , clcIamInstanceProfile      = Just profileName
        , clcImageId                 = ami
        , clcInstanceMonitoring      = Nothing
        , clcInstanceType            = typ
        , clcKernelId                = Nothing
        , clcKeyName                 = Just keyName
        , clcLaunchConfigurationName = appName
        , clcRamdiskId               = Nothing
        , clcSecurityGroups          = Members [groupName, sshGroupName]
        , clcSpotPrice               = Nothing
        , clcUserData                = Nothing
        }
    verifyAS "AlreadyExists" c
    say "Created Launch Configuration {}" [appName]

delete :: Naming a => a -> AWS ()
delete (names -> Names{..}) = do
    void . send $ DeleteLaunchConfiguration appName
    say "Deleted Launch Configuration {}" [appName]
