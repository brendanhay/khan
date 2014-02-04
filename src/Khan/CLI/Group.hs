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

import qualified Data.Text                as Text
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.SecurityGroup as Security
import           Khan.Prelude
import           Network.AWS.EC2

data Group = Group
    { gRole    :: !Role
    , gEnv     :: !Env
    , gAnsible :: !Bool
    }

groupParser :: Parser Group
groupParser = Group
    <$> roleOption
    <*> envOption
    <*> ansibleOption

instance Options Group

instance Naming Group where
    names Group{..} = unversioned gRole gEnv

data Authorise = Authorise
    { aRole    :: !Role
    , aEnv     :: !Env
    , aRules   :: [IpPermissionType]
    , aAnsible :: !Bool
    }

authoriseParser :: Parser Authorise
authoriseParser = Authorise
    <$> roleOption
    <*> envOption
    <*> many (customOption "rule" "RULE" parseRule mempty
        "tcp|udp|icmp:from_port:to_port:[group|0.0.0.0,...]")
    <*> ansibleOption

instance Options Authorise

instance Naming Authorise where
    names Authorise{..} = unversioned aRole aEnv

commands :: Mod CommandFields Command
commands = group "group" "Long description." $ mconcat
    [ command "info" info groupParser
        "Long long long long description."
    , command "create" (modify Security.create) groupParser
        "Long long long long description."
    , command "delete" (modify Security.delete) groupParser
        "Long long long long description."
    , command "authorise" authorise authoriseParser
        "Long long long long description."
    ]

info :: Common -> Group -> AWS ()
info _ (names -> Names{..}) =
    Security.find groupName >>= maybe (return ()) (log_ . format)
  where
    format SecurityGroupItemType{..} = Text.init . Text.unlines $
        [ "OwnerId             = "  <> sgitOwnerId
        , "GroupId             = "  <> sgitGroupId
        , "GroupName           = "  <> sgitGroupName
        , "GroupDescription    = "  <> sgitGroupDescription
        , "VpcId               = "  <> fromMaybe "''" sgitVpcId
        , "IpPermissionsEgress = [" <> showRules sgitIpPermissionsEgress <> "]"
        , "IpPermissions       = [" <> showRules sgitIpPermissions <> "]"
        ]

modify :: (Text -> AWS Bool) -> Common -> Group -> AWS ()
modify f c g@Group{..}
    | not gAnsible = void $ f groupName
    | otherwise    = capture c "security group {}" [groupName] $ f groupName
  where
    Names{..} = names g

authorise :: Common -> Authorise -> AWS ()
authorise c a@Authorise{..}
    | not aAnsible = void $ Security.update groupName aRules
    | otherwise    = capture c "security group {}" [groupName] $
        Security.update groupName aRules
  where
    Names{..} = names a
