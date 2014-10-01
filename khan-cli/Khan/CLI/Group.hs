{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.CLI.Group
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Group (commands) where

import           Khan.Internal
import           Khan.Model.Ansible
import qualified Khan.Model.EC2.SecurityGroup as Security
import           Khan.Prelude
import           Network.AWS.EC2

data Group = Group
    { gRole    :: !Role
    , gEnv     :: !Env
    , gAnsible :: !Bool
    }

groupParser :: EnvMap -> Parser Group
groupParser env = Group
    <$> roleOption
    <*> envOption env
    <*> ansibleOption

instance Options Group where
    validate Group{..} =
        check gEnv "--env must be specified."

instance Naming Group where
    names Group{..} = unversioned gRole gEnv

data Update = Update
    { uRole    :: !Role
    , uEnv     :: !Env
    , uRules   :: [IpPermissionType]
    , uAnsible :: !Bool
    }

updateParser :: EnvMap -> Parser Update
updateParser env = Update
    <$> roleOption
    <*> envOption env
    <*> (uncurry mappend <$> rulesParser)
    <*> ansibleOption
  where
    rulesParser = (,)
        <$> many (customOption "rule" "RULE" parseString mempty
            "Formatted rule. proto:from:to:[group|0.0.0.0,...]")
        <*> customOption "rules" "RULE" (parseDelimited '@') (value [])
            "Multiple rules. Internal use only."

instance Options Update where
    discover _ Common{..} u@Update{..} = return $! u
        { uRules = Security.prefix cRegion uEnv (Security.merge uRules)
        }

instance Naming Update where
    names Update{..} = unversioned uRole uEnv

commands :: EnvMap -> Mod CommandFields Command
commands env = group "group" "Security Groups." $ mconcat
    [ command "info" info (groupParser env)
        "Display information about a security group."
    , command "create" (modify Security.create) (groupParser env)
        "Create a security group."
    , command "delete" (modify Security.delete) (groupParser env)
        "Delete a security group."
    , command "update" update (updateParser env)
        "Update a security group with a new rule set."
    ]

info :: Common -> Group -> AWS ()
info _ (names -> Names{..}) = do
    mg <- Security.find groupName
    maybe (log_ "No Security Groups found.")
          (\g -> pPrintLn $ title g <-> body g)
          mg

modify :: Changed a => (Text -> AWS a) -> Common -> Group -> AWS ()
modify f c g@Group{..} =
    let name = groupName (names g)
     in capture gAnsible c "security group {}" [name] (f name)


update :: Common -> Update -> AWS ()
update c u@Update{..} =
    let name = groupName (names u)
     in capture uAnsible c "security group {}" [name] $
          Security.update name uRules
