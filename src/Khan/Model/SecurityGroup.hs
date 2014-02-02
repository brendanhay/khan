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

find :: Text -> AWS (Maybe SecurityGroupItemType)
find name = do
    log "Searching for Security Group {}" [name]
    mg <- fmap groupMay . sendCatch $ DescribeSecurityGroups [name] [] []
    when (isNothing mg) $ log "Unable to find Security Group {}" [name]
    return mg
  where
    groupMay (Right x) = headMay . toList $ dshrSecurityGroupInfo x
    groupMay (Left  _) = Nothing

create :: Text -> AWS SecurityGroupItemType
create name = find name >>= maybe f return
  where
    f = do
        log "Security Group {} not found, creating..." [name]
        gid <- fmap csgrGroupId . send $ CreateSecurityGroup name name Nothing
        log "Security Group {} created." [gid]
        find name >>= noteAWS "Unable to find created Security Group {}" [name]

-- FIXME: diff causes rules to be revoked before re-adding, due to shallow diff
-- which doesn't inspect the inner UserIdGroupPairs, this could potentially cause
-- a brief netsplit.
update :: Text -> [IpPermissionType] -> AWS Bool
update name (sort -> rules) = do
    grp <- create name

    let gid         = sgitGroupId grp
        fs          = map (UserIdGroupPair Nothing Nothing . uigGroupName)
        gs          = map (\p -> p { iptGroups = fs $ iptGroups p })
        ps          = sort . gs $ sgitIpPermissions grp
        (auth, rev) = diff rules ps

    log "Updating Security Group {}..." [name]

    liftM2 (||) (revoke gid rev) (authorise gid auth)
        <* log "Security Group {} updated." [name]
  where
    revoke gid xs
        | null xs   = return False
        | otherwise = do
            log "Revoking {} on {}..." [showRules xs, name]
            send_ $ RevokeSecurityGroupIngress (Just gid) Nothing xs
            return True

    authorise gid xs
        | null xs   = return False
        | otherwise = do
            log "Authorising {} on {}..." [showRules xs, name]
            es <- sendCatch (AuthorizeSecurityGroupIngress (Just gid) Nothing xs)
            verifyEC2 "InvalidPermission.Duplicate" es
            return $ isRight es

delete :: Text -> AWS Bool
delete name = find name >>= maybe (return False)
    (const $ do
        log "Deleting Security Group {}..." [name]
        send_ $ DeleteSecurityGroup (Just name) Nothing
        log_ "Security Group deleted."
        return True)
