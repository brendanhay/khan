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
import           Khan.Internal.Ansible
import qualified Khan.Model.SecurityGroup as Security
import           Khan.Prelude
import           Network.AWS.EC2

data Group = Group
    { gRole    :: !Role
    , gEnv     :: !Env
    , gRules   :: [IpPermissionType]
    , gAnsible :: !Bool
    }

groupParser :: Parser Group
groupParser = Group
    <$> roleOption
    <*> envOption
    <*> many (customOption "rule" "RULE" parseRule mempty
        "Rule description.")
    <*> ansibleOption

instance Options Group

instance Naming Group where
    names Group{..} = unversioned gRole gEnv

commands :: Mod CommandFields Command
commands = group "group" "Long description." $ mconcat
    [ command "info" info groupParser
        "Long long long long description."
    , command "update" update groupParser
        "Long long long long description."
    , command "delete" delete groupParser
        "Long long long long description."
    ]

info :: Common -> Group -> AWS ()
info _ (names -> Names{..}) = Security.find groupName
    >>= liftIO . maybe (return ()) print

update :: Common -> Group -> AWS ()
update c g@Group{..}
    | not gAnsible = void $ Security.update groupName gRules
    | otherwise    = capture c "security group {}" [groupName] $
        Security.update groupName gRules
  where
    Names{..} = names g

delete :: Common -> Group -> AWS ()
delete c g@Group{..}
    | not gAnsible = void $ Security.delete groupName
    | otherwise    = capture c "security group {}" [groupName] $
        Security.delete groupName
  where
    Names{..} = names g
