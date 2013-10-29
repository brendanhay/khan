{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.CLI.Profile
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Profile (commands) where

import qualified Khan.AWS.IAM  as IAM
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS

defineOptions "Role" $ do
    textOption "rRole" "role" ""
        "Role of the application."

    textOption "rEnv" "env" defaultEnv
        "Environment of the application."

    pathOption "rPolicy" "policy" ""
        "Role policy file."

    pathOption "rTrust" "trust" ""
        "Trust relationship file."

deriving instance Show Role

instance Discover Role where
    discover _ r@Role{..} = do
        (p, t) <- (,)
            <$> defaultPath rPolicy (configPath "policy.json")
            <*> defaultPath rTrust  (configPath "trust.json")
        return $! r { rPolicy = p, rTrust  = t }
      where

instance Validate Role where
    validate Role{..} = do
        check rRole "--name must be specified."
        check rEnv  "--env must be specified."
        checkPath rPolicy " specified by --policy must exist."
        checkPath rTrust  " specified by --trust must exist."

instance Naming Role where
    names Role{..} = unversioned rRole rEnv

commands :: [Command]
commands =
    [ command profile "profile" "Create or update IAM profiles."
        "Stuff."
    ]

profile :: Role -> AWS ()
profile r = IAM.updateRole r (rPolicy r) (rTrust r)
