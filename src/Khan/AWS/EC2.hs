{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.AWS.EC2
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.AWS.EC2 where

import           Control.Arrow          ((***))
import           Control.Concurrent     (threadDelay)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable          (toList)
import           Data.List              ((\\), partition)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           Prelude                hiding (min, max)
import           System.Directory

createKey :: Naming a => a -> AWS ()
createKey (names -> Names{..}) =
    sendCatch (CreateKeyPair keyName) >>= either exist write
  where
    exist e = do
        verifyEC2 "InvalidKeyPair.Duplicate" (Left e)
        logInfo "KeyPair {} exists, not updating." [keyName]

    write k = do
        reg <- currentRegion
        let path = concat [certPath, "/", show reg, ".", Text.unpack keyName, ".pem"]
        liftIO $ do
            createDirectoryIfMissing True certPath
            Text.writeFile path $ ckqKeyMaterial k

        logInfo "Wrote new KeyPair to {}" [path]

findGroup :: Naming a => a -> AWS (Maybe SecurityGroupItemType)
findGroup (names -> Names{..}) = do
    logInfo "Searching for group {}" [groupName]
    mg <- fmap group . sendCatch $ DescribeSecurityGroups [groupName] [] []
    when (isNothing mg) $ logInfo "Unable to find group {}" [groupName]
    return mg
  where
    group (Right x) = headMay . toList $ dshrSecurityGroupInfo x
    group (Left  _) = Nothing

updateGroup :: Naming a => a -> [IpPermissionType] -> AWS ()
updateGroup (names -> n@Names{..}) rules =
    findGroup n >>= maybe (create >>= modify) modify
  where
    create = do
        logInfo "{} not found, creating..." [groupName]
        gid <- fmap csgrGroupId . send $
            CreateSecurityGroup groupName groupName Nothing
        logInfo "Group {} created." [gid]
        findGroup n >>= noteErrorF "Unable to find created group {}" [groupName]

    modify grp = do
        logInfo "Updating group {}..." [groupName]

        let gid  = sgitGroupId grp
            strip1 = map (UserIdGroupPair Nothing Nothing . uigGroupName)
            strip2 = map (\p -> p { iptGroups = strip1 . toList $ iptGroups p })

            ps   = toList $ sgitIpPermissions grp
            auth = strip2 rules \\ strip2 ps
            rev  = strip2 ps \\ strip2 rules

        unless (null auth) $ do
            logInfo "Authorizing {} on {}..." [showRules auth, groupName]
            send_ $ AuthorizeSecurityGroupIngress (Just gid) Nothing auth

        unless (null rev) $ do
            logInfo "Revoking {} on {}..." [showRules rev, groupName]
            send_ $ RevokeSecurityGroupIngress (Just gid) Nothing rev

        logInfo "Group {} updated." [groupName]

deleteGroup :: Naming a => a -> AWS ()
deleteGroup (names -> Names{..}) = do
    logInfo "Deleting group {}..." [groupName]
    send_ $ DeleteSecurityGroup (Just groupName) Nothing
    logInfo_ "Group deleted."

findInstances :: [Text] -> AWS [RunningInstancesItemType]
findInstances ids = fmap (concatMap ritInstancesSet . dirReservationSet) .
    send $ DescribeInstances ids []

runInstances :: Naming a
             => a
             -> Text
             -> InstanceType
             -> Integer
             -> Integer
             -> Text
             -> Bool
             -> AWS [Text]
runInstances (names -> Names{..}) image typ min max ud opt =
    fmap (map riitInstanceId . rirInstancesSet) . send $ RunInstances
        image
        min
        max
        (Just keyName)
        []                            -- Group Ids
        [groupName, sshGroup envName] -- Group Names
        (Just ud)                     -- User Data
        (Just typ)
        Nothing
        Nothing
        Nothing
        []                            -- Block Devices
        (Just $ MonitoringInstanceType True)
        Nothing
        Nothing                       -- FIXME: Disable API Termination
        Nothing                       -- Shutdown Behaviour
        Nothing                       -- Private IP
        Nothing                       -- llient Token
        []                            -- NICs
        [IamInstanceProfileRequestType Nothing (Just profileName)]
        (Just opt)

tagInstances :: Naming a => a -> [Text] -> AWS ()
tagInstances (names -> Names{..}) ids = do
    logInfo_ "Tagging instances with Group, Role, and Env..."
    send_ $ CreateTags ids
        [ ResourceTagSetItemType "Group" groupName
        , ResourceTagSetItemType "Role" roleName
        , ResourceTagSetItemType "Env" envName
        ]

waitForInstances :: [Text] -> AWS ()
waitForInstances []  = logInfo_ "All instances running"
waitForInstances ids = do
    xs <- findInstances ids

    let (ps, rs) = join (***) (map riitInstanceId) $ pending xs

    unless (null rs) $ logInfo "Instances marked as running: {}" [format rs]

    unless (null ps) $ do
        logInfo "Instances still pending: {}" [format ps]
        logInfo_ "Waiting..."
        liftIO . threadDelay $ 1000000 * 10

    waitForInstances ps
  where
    pending = partition (("pending" ==) . istName . riitInstanceState)

    format  = Text.intercalate ", "

findImage :: Naming a => a -> AWS Text
findImage (names -> Names{..}) = do
    rs  <- fmap (listToMaybe . djImagesSet) . send $
        DescribeImages [] [] ["self"] [Filter "name" [imageName]]
    ami <- fmap diritImageId $ noteError "Failed to find any matching AMIs" rs
    logInfo "Found AMI {} named {}" [ami, imageName]
    return ami
