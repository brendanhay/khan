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

import           Control.Monad
import           Khan.Internal
import qualified Khan.Model.EC2.SecurityGroup as Security
import           Khan.Prelude                 hiding (min, max)
import           Network.AWS.AutoScaling      hiding (Filter)

create :: Naming a => a -> Text -> InstanceType -> AWS ()
create (names -> n@Names{..}) ami typ = do
    say "Creating Launch Configuration {}" [appName]
    gs <- Security.defaults n
    c  <- sendCatch CreateLaunchConfiguration
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
        , clcSecurityGroups          = Members gs
        , clcSpotPrice               = Nothing
        , clcUserData                = Nothing
        }
    verifyAS "AlreadyExists" c

delete :: Naming a => a -> AWS ()
delete (names -> Names{..}) = do
    say "Deleting Launch Configuration {}" [appName]
    void . send $ DeleteLaunchConfiguration appName
