{-# LANGUAGE NoImplicitPrelude #-}
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

import           Control.Arrow       ((***))
import           Control.Concurrent  (threadDelay)
import           Data.List           ((\\), partition)
import qualified Data.Text           as Text
import           Khan.Internal
import           Khan.Prelude        hiding (min, max)
import           Network.AWS
import           Network.AWS.EC2
import qualified Shelly              as Shell

keyPath :: Text -> FilePath -> AWS FilePath
keyPath name dir = do
    reg <- Text.pack . show <$> currentRegion
    liftEitherT
        . sh
        . Shell.absPath
        . (dir </>)
        . Shell.fromText
        $ Text.concat [reg, ".", name, ".pem"]

createKey :: Naming a => a -> FilePath -> AWS ()
createKey (names -> Names{..}) dir =
    sendCatch (CreateKeyPair keyName) >>= either exist write
  where
    exist e = do
        verifyEC2 "InvalidKeyPair.Duplicate" (Left e)
        logInfo "KeyPair {} exists, not updating." [keyName]

    write k = do
        f <- keyPath keyName dir
        shell $ Shell.mkdir_p dir >> Shell.writefile f (ckqKeyMaterial k)
        logInfo "Wrote new KeyPair to {}" [f]

findGroup :: Naming a => a -> AWS (Maybe SecurityGroupItemType)
findGroup (names -> Names{..}) = do
    logInfo "Searching for group {}" [groupName]
    mg <- fmap groupMay . sendCatch $ DescribeSecurityGroups [groupName] [] []
    when (isNothing mg) $ logInfo "Unable to find group {}" [groupName]
    return mg
  where
    groupMay (Right x) = headMay . toList $ dshrSecurityGroupInfo x
    groupMay (Left  _) = Nothing

updateGroup :: Naming a => a -> [IpPermissionType] -> AWS ()
updateGroup (names -> n@Names{..}) rules =
    findGroup n >>= maybe (create >>= modify) modify
  where
    create = do
        logInfo "{} not found, creating..." [groupName]
        gid <- fmap csgrGroupId . send $
            CreateSecurityGroup groupName groupName Nothing
        logInfo "Group {} created." [gid]
        findGroup n >>= noteFormat "Unable to find created group {}" [groupName]

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

findInstances :: [Text] -> [Filter] -> AWS [RunningInstancesItemType]
findInstances ids = fmap (concatMap ritInstancesSet . dirReservationSet) .
    send . DescribeInstances ids

runInstances :: Naming a
             => a
             -> Text
             -> InstanceType
             -> AvailabilityZone
             -> Integer
             -> Integer
             -> Text
             -> Bool
             -> AWS [RunningInstancesItemType]
runInstances (names -> Names{..}) image typ az min max ud opt =
    fmap rirInstancesSet . send $ RunInstances
        image
        min
        max
        (Just keyName)
        []                            -- Group Ids
        [groupName, sshGroup envName] -- Group Names
        (Just ud)                     -- User Data
        (Just typ)
        (Just $ PlacementType (Just az) Nothing Nothing)
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

tagInstances :: Naming a => a -> Text -> [Text] -> AWS ()
tagInstances (names -> n) dom ids = do
    logInfo_ "Tagging instances with Group, Role, and Env..."
    send_ . CreateTags ids . map (uncurry ResourceTagSetItemType) $
        defaultTags n dom

waitForInstances :: [Text] -> AWS ()
waitForInstances []  = logInfo_ "All instances running"
waitForInstances ids = do
    xs <- findInstances ids []

    let (ps, rs) = join (***) (map riitInstanceId) $ pending xs

    unless (null rs) $ logInfo "Instances marked as running: {}" [format rs]

    unless (null ps) $ do
        logInfo "Instances still pending: {}" [format ps]
        logInfo_ "Waiting..."
        liftIO . threadDelay $ 1000000 * 30

    waitForInstances ps
  where
    pending = partition (("pending" ==) . istName . riitInstanceState)

    format  = Text.intercalate ", "

findImage :: [Text] -> AWS Text
findImage images = do
    logInfo "Finding AMIs matching: {}" [options]
    rs  <- fmap (listToMaybe . djImagesSet) . send $
        DescribeImages [] [] ["self"] [Filter "name" images]
    ami <- fmap diritImageId . hoistError $
        note "Failed to find any matching AMIs" rs
    logInfo "Found AMI {} matching {}" [ami, options]
    return ami
  where
    options = Text.intercalate " | " images

findCurrentZones :: AWS [AvailabilityZoneItemType]
findCurrentZones = do
    reg <- Text.pack . show <$> currentRegion
    logInfo_ "Finding AZs in current region"
    fmap dazrAvailabilityZoneInfo . send $
        DescribeAvailabilityZones []
            [ Filter "region-name" [reg]
            , Filter "state" ["available"]
            ]
