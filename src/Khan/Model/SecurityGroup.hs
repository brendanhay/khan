{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Model.SecurityGroup
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.SecurityGroup
    (
    -- * SSH group enforcement
      sshGroup

    -- * EC2 API calls
    , find
    , create
    , update
    , delete
    ) where

import Control.Monad
import Data.List       (sort)
import Khan.Internal
import Khan.Prelude    hiding (find, min, max)
import Network.AWS.EC2 hiding (Instance)

sshGroup :: Naming a => a -> AWS Bool
sshGroup = flip update rules . sshGroupName . names
  where
    rules = [ IpPermissionType TCP 22 22 [] [IpRange "0.0.0.0/0"]
            ]

find :: Naming a => a -> AWS (Maybe SecurityGroupItemType)
find (names -> Names{..}) = do
    log "Searching for Security Group {}" [groupName]
    mg <- fmap groupMay . sendCatch $ DescribeSecurityGroups [groupName] [] []
    when (isNothing mg) $ log "Unable to find Security Group {}" [groupName]
    return mg
  where
    groupMay (Right x) = headMay . toList $ dshrSecurityGroupInfo x
    groupMay (Left  _) = Nothing

create :: Naming a => a -> AWS SecurityGroupItemType
create (names -> n@Names{..}) = find n >>= maybe f return
  where
    f = do
        log "Security Group {} not found, creating..." [groupName]
        gid <- fmap csgrGroupId . send $
            CreateSecurityGroup groupName groupName Nothing
        log "Security Group {} created." [gid]
        find n >>= noteAWS "Unable to find created Security Group {}" [groupName]

-- FIXME: diff causes rules to be revoked before re-adding, due to shallow diff
-- which doesn't inspect the inner UserIdGroupPairs, this could potentially cause
-- a brief netsplit.
update :: Naming a => a -> [IpPermissionType] -> AWS Bool
update (names -> n@Names{..}) (sort -> rules) = do
    grp <- create n

    let gid         = sgitGroupId grp
        fs          = map (UserIdGroupPair Nothing Nothing . uigGroupName)
        gs          = map (\p -> p { iptGroups = fs $ iptGroups p })
        ps          = sort . gs $ sgitIpPermissions grp
        (auth, rev) = diff rules ps

    log "Updating Security Group {}..." [groupName]

    liftM2 (||) (revoke gid rev) (authorise gid auth)
        <* log "Security Group {} updated." [groupName]
  where
    revoke gid xs
        | null xs   = return False
        | otherwise = do
            log "Revoking {} on {}..." [showRules xs, groupName]
            send_ $ RevokeSecurityGroupIngress (Just gid) Nothing xs
            return True

    authorise gid xs
        | null xs   = return False
        | otherwise = do
            log "Authorising {} on {}..." [showRules xs, groupName]
            es <- sendCatch (AuthorizeSecurityGroupIngress (Just gid) Nothing xs)
            verifyEC2 "InvalidPermission.Duplicate" es
            return $ isRight es

delete :: Naming a  => a -> AWS Bool
delete (names -> n@Names{..}) = find n >>= maybe (return False)
    (const $ do
        log "Deleting Security Group {}..." [groupName]
        send_ $ DeleteSecurityGroup (Just groupName) Nothing
        log_ "Security Group deleted."
        return True)
