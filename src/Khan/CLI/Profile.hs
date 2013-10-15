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

module Khan.CLI.Profile (cli) where

import qualified Khan.AWS.IAM           as IAM
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Text.Show.Pretty

defineOptions "Role" $ do
    textOption "rName" "name" ""
        "Name of the application."

    textOption "rEnv" "env" defaultEnv
        "Environment of the application."

    pathOption "rPolicy" "policy" policyPath
        "Role policy file."

    pathOption "rTrust" "trust" trustPath
        "Trust relationship file."

deriving instance Show Role

instance Discover Role

instance Validate Role where
    validate Role{..} = do
        check rName "--name must be specified."
        check rEnv  "--env must be specified."
        checkPath rPolicy " specified by --policy must exist."
        checkPath rTrust  " specified by --trust must exist."

instance Naming Role where
    names Role{..} = unversioned rName rEnv

cli :: Command
cli = Command "profile" "Manage IAM Roles and Profiles."
    [ subCommand "info"   info
    , subCommand "create" create
    , subCommand "delete" delete
    ]

info :: Role -> AWS ()
info r = IAM.findRole r >>= liftIO . putStrLn . ppShow

create :: Role -> AWS ()
create r = IAM.updateRole r (rPolicy r) (rTrust r)

delete :: Role -> AWS ()
delete Role{..} = return ()
