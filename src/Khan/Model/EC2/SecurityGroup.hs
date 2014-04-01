{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Model.EC2.SecurityGroup
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.EC2.SecurityGroup
    (
    -- * SSH rules
      sshAccess

    -- * Default groups
    , defaults
    , createGroups

    -- * EC2 API calls
    , find
    , create
    , delete
    , update
    ) where

import Control.Monad          (liftM2)
import Data.List              (sort)
import Khan.Internal   hiding (Protocol(..))
import Khan.Prelude    hiding (find, min, max)
import Network.AWS.EC2 hiding (Instance)

sshAccess :: Naming a => a -> AWS Bool
sshAccess (names -> Names{..}) = update groupName
    [ IpPermissionType TCP 22 22 [] [IpRange "0.0.0.0/0"]
    ]

defaults :: Naming a => a -> AWS [Text]
defaults (names -> Names{..}) =
    (: [envName, groupName]) . abbreviate <$> getRegion

createGroups :: Naming a => a -> AWS Bool
createGroups = fmap (any modified) . mapM create <=< defaults

find :: Text -> AWS (Maybe SecurityGroupItemType)
find name = do
    say "Searching for Security Group {}" [name]
    groupMay <$> sendCatch (DescribeSecurityGroups [name] [] [])
  where
    groupMay (Right x) = headMay . toList $ dshrSecurityGroupInfo x
    groupMay (Left  _) = Nothing

create :: Text -> AWS (Modified SecurityGroupItemType)
create name = find name >>= maybe go (return . Unchanged)
  where
    go = do
        say "Security Group {} not found, creating..." [name]
        gid <- csgrGroupId <$> send (CreateSecurityGroup name name Nothing)
        gs  <- dshrSecurityGroupInfo <$> send (DescribeSecurityGroups [] [gid] [])
        grp <- noteAWS "Unable to find created Security Group {}" [B name] (headMay gs)
        say "Security Group {}/{} created." [name, gid]
        return $ Changed grp

delete :: Text -> AWS Bool
delete name = find name >>= maybe (return False) (const go)
  where
    go = do
        say "Deleting Security Group {}..." [name]
        send_ $ DeleteSecurityGroup (Just name) Nothing
        log_ "Security Group deleted."
        return True

-- FIXME: diff causes rules to be revoked before re-adding, due to shallow diff
-- which doesn't inspect the inner UserIdGroupPairs, this could potentially cause
-- a brief netsplit.
update :: Text -> [IpPermissionType] -> AWS Bool
update name (sort -> rules) = do
    SecurityGroupItemType{..} <- result <$> create name

    let fs          = map (UserIdGroupPair Nothing Nothing . uigGroupName)
        gs          = map (\p -> p { iptGroups = fs $ iptGroups p })
        ps          = sort (gs sgitIpPermissions)
        (auth, rev) = diff rules ps

    say "Updating Security Group {}..." [name]

    liftM2 (||) (revoke sgitGroupId rev) (authorise sgitGroupId auth)
        <* say "Security Group {} updated." [name]
  where
    revoke gid xs
        | null xs   = return False
        | otherwise = do
            log "Revoking {} on {}..." [B xs, B name]
            send_ $ RevokeSecurityGroupIngress (Just gid) Nothing xs
            return True

    authorise gid xs
        | null xs   = return False
        | otherwise = do
            log "Authorising {} on {}..." [B xs, B name]
            es <- sendCatch (AuthorizeSecurityGroupIngress (Just gid) Nothing xs)
            verifyEC2 "InvalidPermission.Duplicate" es
            return $ isRight es
