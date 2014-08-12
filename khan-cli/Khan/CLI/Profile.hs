{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

import           Khan.Internal
import           Khan.Model.IAM.Role (Paths(..))
import qualified Khan.Model.IAM.Role as Role
import           Khan.Prelude
import           Network.AWS.IAM hiding (Role)

default (Text)

data Info = Info
    { iRole   :: !Role
    , iEnv    :: !Env
    } deriving (Show)

infoParser :: EnvMap -> Parser Info
infoParser env = Info
    <$> roleOption
    <*> envOption env

instance Options Info where
    validate Info{..} =
        check iEnv "--env must be specified."

instance Naming Info where
    names Info{..} = unversioned iRole iEnv

data Update = Update
    { uRole   :: !Role
    , uEnv    :: !Env
    , uTrust  :: !TrustPath
    , uPolicy :: !PolicyPath
    } deriving (Show)

updateParser :: EnvMap -> Parser Update
updateParser env = Update
    <$> roleOption
    <*> envOption env
    <*> trustOption
    <*> policyOption

instance Options Update where
    discover _ Common{..} r@Update{..} = return $! r
        { uTrust  = pTrustPath
        , uPolicy = pPolicyPath
        }
      where
        Paths{..} = Role.paths r cConfig uTrust uPolicy

    validate Update{..} = do
        check uEnv "--env must be specified."
        checkFile (_trust  uTrust)  " specified by --trust must exist."
        checkFile (_policy uPolicy) " specified by --policy must exist."

instance Naming Update where
    names Update{..} = unversioned uRole uEnv

commands :: EnvMap -> Mod CommandFields Command
commands env = group "profile" "IAM Updates and Roles." $ mconcat
    [ command "info" info (infoParser env)
        "Display information about an IAM Role."
    , command "update" update (updateParser env)
        "Create or update IAM Role and associated Role."
    ]

info :: Common -> Info -> AWS ()
info _ i = do
    ra <- async (Role.find i)
    pa <- async (Role.findPolicy i)
    r  <- wait ra
    p  <- wait pa
    pPrint (title r <-> body r <-> body p)

update :: Common -> Update -> AWS ()
update _ u = void $ Role.update u (uTrust u) (uPolicy u)
