{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.LaunchConfig
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.LaunchConfig
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
        (Members [])
        Nothing
        (Just profileName)                  -- Instance Profile
        ami                                 -- Image Id
        Nothing
        typ                                 -- Instance Type
        Nothing
        (Just keyName)                      -- Key Pair Name
        appName                             -- Launch Configuration Name
        Nothing
        (Members [groupName, sshGroupName]) -- Security Groups
        Nothing
        Nothing                             -- User Data
    verifyAS "AlreadyExists" c
    say "Created Launch Configuration {}" [appName]

delete :: Naming a => a -> AWS ()
delete (names -> Names{..}) = do
    void . send $ DeleteLaunchConfiguration appName
    say "Deleted Launch Configuration {}" [appName]
