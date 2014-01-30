{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Khan.Model.Profile
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Profile
    ( Policy (..)
    , policy
    , find
    , update
    ) where

import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude              hiding (find)
import           Network.AWS.IAM
import qualified Shelly                    as Shell

default (Text)

data Policy = Policy
    { pTrustPath  :: !FilePath
    , pPolicyPath :: !FilePath
    }

policy :: Naming a
       => a
       -> FilePath -- ^ Config directory
       -> FilePath -- ^ Trust file path
       -> FilePath -- ^ Policy file path
       -> Policy
policy (names -> Names{..}) root t p = Policy
    { pTrustPath  = defaultPath t trust
    , pPolicyPath = defaultPath p policy
    }
  where
    trust  = root </> Path.fromText "trust.json"
    policy = root </> "policies" </> Path.fromText policyName <.> "json"

find :: Naming a => a -> AWS Role
find = fmap (grrRole . grrGetRoleResult) . send . GetRole . profileName . names

update :: Naming a => a -> FilePath -> FilePath -> AWS Role
update (names -> n@Names{..}) ppath tpath = do
    (p, t) <- shell $ (,)
        <$> Shell.readfile ppath
        <*> Shell.readfile tpath

    i <- sendAsync $ CreateInstanceProfile profileName Nothing
    r <- sendAsync $ CreateRole t Nothing profileName

    wait i >>= verifyIAM "EntityAlreadyExists"
    wait r >>= verifyIAM "EntityAlreadyExists"

    a <- sendAsync $ AddRoleToInstanceProfile profileName profileName
    p <- sendAsync $ PutRolePolicy p profileName profileName

    wait a >>= verifyIAM "LimitExceeded"
    waitAsync_ p <* log "Updated policy for Role {}" [profileName]

    find n
