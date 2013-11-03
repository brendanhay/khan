{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

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

data Role = Role
    { rRole   :: !Text
    , rEnv    :: !Text
    , rPolicy :: !FilePath
    , rTrust  :: !FilePath
    } deriving (Show)

roleParser :: Parser Role
roleParser = Role
    <$> defineText "role" "ROLE" "" "Role of the application"
    <*> defineText "env" "STR" defaultEnv "Environment of the application."
    <*> definePath "policy" "PATH" "" "Role policy file."
    <*> definePath "trust" "PATH" "" "Trust relationship file."

instance Options Role where
    discover r@Role{..} = do
        (p, t) <- (,)
            <$> defaultPath rPolicy (configPath "policy.json")
            <*> defaultPath rTrust  (configPath "trust.json")
        return $! r { rPolicy = p, rTrust  = t }

    validate Role{..} = do
        check rRole "--name must be specified."
        check rEnv  "--env must be specified."
        checkPath rPolicy " specified by --policy must exist."
        checkPath rTrust  " specified by --trust must exist."

instance Naming Role where
    names Role{..} = unversioned rRole rEnv

commands :: Mod CommandFields Command
commands = group "profile" "Long description."
     $ command "profile" update roleParser "Create or update IAM profiles."
  where
    update _ r = IAM.updateRole r (rPolicy r) (rTrust r)
