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

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.Text.IO           as Text
import           Khan.Internal
import           Network.AWS
import           Network.AWS.IAM

findRole :: Naming a => a -> AWS Role
findRole = fmap (grrRole . grrGetRoleResult)
     . send
     . GetRole
     . groupName
     . names

updateRole :: Naming a => a -> FilePath -> FilePath -> AWS ()
updateRole (names -> Names{..}) ppath tpath = do
    (policy, trust) <- liftIO $ (,)
        <$> Text.readFile ppath
        <*> Text.readFile tpath

    i <- sendAsync $ CreateInstanceProfile roleName Nothing
    r <- sendAsync $ CreateRole trust Nothing roleName

    wait i >>= verifyIAM "EntityAlreadyExists"
    wait r >>= verifyIAM "EntityAlreadyExists"

    a <- sendAsync $ AddRoleToInstanceProfile roleName roleName
    p <- sendAsync $ PutRolePolicy policy roleName roleName

    wait a >>= verifyIAM "LimitExceeded"
    waitAsync_ p <* logInfo "Updated policy for Role {}" [roleName]
