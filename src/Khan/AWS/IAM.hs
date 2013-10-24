{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.AWS.IAM
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.AWS.IAM where

import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.IAM
import qualified Shelly          as Shell

findRole :: Naming a => a -> AWS Role
findRole = fmap (grrRole . grrGetRoleResult) .
    send . GetRole . groupName . names

updateRole :: Naming a => a -> FilePath -> FilePath -> AWS ()
updateRole (names -> Names{..}) ppath tpath = do
    (policy, trust) <- shell $ (,)
        <$> Shell.readfile ppath
        <*> Shell.readfile tpath

    i <- sendAsync $ CreateInstanceProfile profileName Nothing
    r <- sendAsync $ CreateRole trust Nothing profileName

    wait i >>= verifyIAM "EntityAlreadyExists"
    wait r >>= verifyIAM "EntityAlreadyExists"

    a <- sendAsync $ AddRoleToInstanceProfile profileName profileName
    p <- sendAsync $ PutRolePolicy policy profileName profileName

    wait a >>= verifyIAM "LimitExceeded"
    waitAsync_ p <* log "Updated policy for Role {}" [profileName]
