{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Khan.Model.Role
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Role
    ( Paths (..)
    , paths
    , find
    , findPolicy
    , update
    ) where

import           Data.Aeson
import qualified Data.Text.Lazy            as LText
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal             hiding (Role)
import           Khan.Prelude              hiding (find)
import           Network.AWS.IAM

default (Text)

data Paths = Paths
    { pTrustPath  :: !TrustPath
    , pPolicyPath :: !PolicyPath
    }

paths :: Naming a
      => a
      -> ConfigDir
      -> TrustPath  -- ^ Trust template path
      -> PolicyPath -- ^ Policy template path
      -> Paths
paths (names -> Names{..}) (ConfigDir root) t p = Paths
    { pTrustPath  = TrustPath $ defaultPath (_trust t) tpath
    , pPolicyPath = PolicyPath $ defaultPath (_policy p) ppath
    }
  where
    tpath = root </> Path.fromText "trust.ede"
    ppath = root </> "policies" </> Path.fromText roleName <.> "ede"

find :: Naming a => a -> AWS Role
find (names -> Names{..}) = do
    say "Finding IAM Role {}" [profileName]
    grrRole . grrGetRoleResult <$> send (GetRole profileName)

findPolicy :: Naming a => a -> AWS GetRolePolicyResult
findPolicy (names -> Names{..}) = do
    say "Finding IAM Policy for Role {}" [profileName]
    grprGetRolePolicyResult <$> send (GetRolePolicy profileName profileName)

update :: Naming a => a -> TrustPath -> PolicyPath -> AWS Role
update (names -> n@Names{..}) tpath ppath = do
    (t, p) <- (,)
        <$> renderTemplate o (_trust  tpath)
        <*> renderTemplate o (_policy ppath)

    i <- sendAsync $ CreateInstanceProfile profileName Nothing
    r <- sendAsync $ CreateRole (LText.toStrict t) Nothing profileName

    wait i >>= verifyIAM "EntityAlreadyExists"
    wait r >>= verifyIAM "EntityAlreadyExists"

    ar <- sendAsync $ AddRoleToInstanceProfile profileName profileName
    pr <- sendAsync $ PutRolePolicy (LText.toStrict p) profileName profileName

    wait ar >>= verifyIAM "LimitExceeded"
    waitAsync_ pr <* say "Updated policy for Role {}" [profileName]

    find n
  where
    Object o = toJSON n
