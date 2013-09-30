{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Application
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Application (command) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Version
import           Khan.Internal
import           Network.AWS
import           Network.AWS.AutoScaling  hiding (Filter)
import           Network.AWS.EC2
import           Network.AWS.IAM
import           Network.AWS.Internal
import           Text.Printf

defineOptions "App" $ do
    textOption "aName" "name" ""
        "Name of the application."

    textOption "aEnv" "env" defaultEnv
        "Environment of the application."

    stringOption "aPolicy" "policy" "./role-policy.json"
        "Role policy file."

    stringOption "aTrust" "trust" "./trust-relationship.json"
        "Trust relationship file."

deriving instance Show App

instance Discover App

instance Validate App where
    validate App{..} = do
        check aName "--name must be specified."
        check aEnv  "--env must be specified."
        checkPath aPolicy $ aPolicy ++ " specified by --policy must exist."
        checkPath aTrust  $ aTrust ++ " specified by --trust must exist."

defineOptions "Deploy" $ do
    textOption "dName" "name" ""
        "Name of the application."

    textOption "dEnv" "env" defaultEnv
        "Environment to deploy the application into."

    versionOption "dVersion" "version" defaultVersion
        "Version of the application."

    zonesOption "dZones" "zones"
        "Availability zones in which instances are provisioned. separator: ,"

    integerOption "dGrace" "grace" 20
        "Seconds until healthchecks are activated."

    integerOption "dMin" "min" 1
        "Minimum number of instances."

    integerOption "dMax" "max" 20
        "Maximum number of instances."

    integerOption "dCapacity" "desired" 2
        "Desired number of instances."

    integerOption "dCooldown" "cooldown" 60
        "Seconds between scaling activities."

    instanceTypeOption "dType" "instance" M1_Medium
        "Type of instance to provision."

deriving instance Show Deploy

instance Discover Deploy

instance Validate Deploy where
    validate Deploy{..} = do
        check dName     "--name must be specified."
        check dEnv      "--env must be specified."
        check dZones    "--zone must be specified at least once."
        check dGrace    "--grace must be greater than 0."
        check dMin      "--min must be greater than 0."
        check dMax      "--max must be greater than 0."
        check dCapacity "--desired must be greater than 0."
        check dCooldown "--cooldown must be greater than 0"
        check (defaultVersion == dVersion) "--version must be specified."

defineOptions "Cluster" $ do
    textOption "cName" "name" ""
        "Name of the application."

    textOption "cEnv" "env" defaultEnv
        "Environment to deploy the application into."

    versionOption "cVersion" "version" defaultVersion
        "Version of the application."

deriving instance Show Cluster

instance Discover Cluster

instance Validate Cluster where
    validate Cluster{..} = do
        check cName "--name must be specified."
        check cEnv  "--env must be specified."
        check (defaultVersion == cVersion) "--version must be specified."

command :: Command
command = Command "app" "Manage Applications."
    [ subCommand "create" create
    , subCommand "deploy" deploy
    , subCommand "scale"  scale
    , subCommand "retire" retire
    , subCommand "info"   info
    , subCommand "route"  route
    ]
  where
    create App{..} = do
        (policy, trust) <- liftIO $ (,)
            <$> Text.readFile aPolicy
            <*> Text.readFile aTrust

        i <- sendAsync $ CreateInstanceProfile role Nothing
        r <- sendAsync $ CreateRole trust Nothing role
        k <- sendAsync $ CreateKeyPair role

        wait i >>= checkError (("EntityAlreadyExists" ==) . etCode . erError)
        wait r >>= checkError (("EntityAlreadyExists" ==) . etCode . erError)
        wait k >>= either exist write

        -- Write key and upload to special S3 bucket?

        a <- sendAsync $ AddRoleToInstanceProfile role role
        p <- sendAsync $ PutRolePolicy policy role role

        -- etCode = "LimitExceeded",
        -- etMessage = "Cannot exceed quota for InstanceSessionsPerInstanceProfile: 1"
        wait a >>= checkError (("LimitExceeded" ==) . etCode . erError)
        waitAsync_ p <* logInfo "Updated policy for role {}" [role]
      where
        role = roleName aName aEnv
        path = Text.unpack role ++ ".pem"

        exist e = do
            checkError
                (("InvalidKeyPair.Duplicate" ==) . ecCode . head . eerErrors)
                (Left e)
            logInfo "KeyPair {} exists, not updating." [role]

        write k = do
            liftIO . Text.writeFile path $ ckqKeyMaterial k
            logInfo "Wrote new KeyPair to {}" [path]

    deploy Deploy{..} = do
        g <- send $ DescribeAutoScalingGroups (Members [name]) Nothing Nothing

        -- FIXME: Check if a delete is in progress and block

        when (not . null . members .  dasgrAutoScalingGroups $ dashrDescribeAutoScalingGroupsResult g) .
            throwError . printf "Auto Scaling Group %s already exists." $ Text.unpack name

        r <- sendAsync $ GetRole role
        k <- sendAsync $ DescribeKeyPairs [role] []
        a <- sendAsync $ DescribeImages [] [] ["self"] [Filter "name" [image]]
        s <- sendAsync $ CreateSecurityGroup role role Nothing

        waitAsync_ r <* logInfo "Found Role {}" [role]
        waitAsync_ k <* logInfo "Found KeyPair {}" [role]

        ami <- (listToMaybe . djImagesSet <$> waitAsync a) >>=
            (fmap diritImageId . noteError "Failed to find any matching AMIs")
        logInfo "Found AMI {} named {}" [ami, image]

        wait s >>= checkError (("InvalidGroup.Duplicate" ==) . ecCode . head . eerErrors)
        logInfo "Found Security Group {}" [role]

        i <- sendAsync $ AuthorizeSecurityGroupIngress Nothing (Just role)
            [ (IpPermissionType TCP 8080 8080)
                [UserIdGroupPair Nothing Nothing (Just role)] []
            ]
        c <- sendAsync $ CreateLaunchConfiguration
            (Members [])
            Nothing
            (Just role)      -- IAM Role
            ami              -- Image Id
            Nothing
            dType            -- Instance Type
            Nothing
            (Just role)      -- Key Pair Name
            name             -- Launch Configuration Name
            Nothing
            (Members [role]) -- Security Groups
            Nothing
            Nothing          -- User Data

        wait i >>= checkError (("InvalidPermission.Duplicate" ==) . ecCode . head . eerErrors)
        logInfo_ "Updated Security Group rules"

        wait c >>= checkError (("AlreadyExists" ==) . aseCode . aserError)
        logInfo "Created Launch Configuration {}" [name]

        send_ $ CreateAutoScalingGroup
            name                        -- Name
            (Members dZones)            -- Zones
            (Just dCooldown)            -- Default Cooldown
            (Just dCapacity)            -- Desired Capacity
            (Just dGrace)               -- Grace Period
            (Just "EC2")                -- Health Check Type: EC2 | ELB
            name                        -- Launch Configuration Name
            (Members [])
            dMax
            dMin
            Nothing
            (Members [tag "Name" name]) -- Tags
            (Members [])
            Nothing
        logInfo "Created Auto Scaling Group {}" [name]
      where
        image = versionName dName dVersion
        name  = versionName role dVersion
        role  = roleName dName dEnv

        tag k v = Tag k (Just True) (Just name) (Just "auto-scaling-group") (Just v)

    scale Deploy{..} = do
        g   <- send $ DescribeAutoScalingGroups (Members [name]) Nothing Nothing
        grp <- noteError (printf "Auto Scaling Group %s doesn't exist." $ Text.unpack name)
            . listToMaybe . members .  dasgrAutoScalingGroups $
                dashrDescribeAutoScalingGroupsResult g

        liftIO $ print grp

        -- send_ $ UpdateAutoScalingGroup
        --     name             -- Name
        --     (Members dZones) -- Zones
        --     (Just dCooldown) -- Default Cooldown
        --     (Just dCapacity) -- Desired Capacity
        --     (Just dGrace)    -- Grace Period
        --     (Just "EC2")     -- Health Check Type: EC2 | ELB
        --     (Just name)      -- Launch Configuration Name
        --     (Just dMax)
        --     (Just dMin)
        --     Nothing
        --     (Members [])
        --     Nothing
        -- logInfo $ "Updated Auto Scaling Group " ++ Text.unpack name
      where
        name = versionName role dVersion
        role = roleName dName dEnv

    retire Cluster{..} = do
        send_ $ DeleteAutoScalingGroup name (Just True)
        logInfo "Delete of Auto Scaling Group {} in progress" [name]
        send_ $ DeleteLaunchConfiguration name
        logInfo "Deleted Launch Configuration {}" [name]
      where
        name = versionName role cVersion
        role = roleName cName cEnv

    info Cluster{..} = return ()

    route Cluster{..} = return ()

roleName :: Text -> Text -> Text
roleName name env = Text.concat [name, "-", env]

versionName :: Text -> Version -> Text
versionName name ver = Text.concat [name, "_", safeVersion ver]
