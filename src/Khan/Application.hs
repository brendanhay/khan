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
import           Control.Concurrent      (threadDelay)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Version
import           Khan.Internal
import           Network.AWS
import           Network.AWS.AutoScaling hiding (Filter)
import           Network.AWS.EC2
import           Network.AWS.IAM
import           Network.AWS.Internal

data ASG = ASG
    { name  :: Text
    , image :: Text
    , role  :: Text
    }

versioned :: Text -> Text -> Version -> ASG
versioned app env ver = ASG name image role
  where
    name  = Text.concat [role, "_", safeVersion ver]
    image = Text.concat [app, "_", safeVersion ver]
    role  = Text.concat [app,  "-", env]

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

    stringOption "dZones" "zones" "abc"
         "Availability zones suffixes in which instances are provisioned."

    integerOption "dGrace" "grace" 20
        "Seconds until healthchecks are activated."

    integerOption "dMin" "min" 1
        "Minimum number of instances."

    integerOption "dMax" "max" 20
        "Maximum number of instances."

    integerOption "dDesired" "desired" 2
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
        check dGrace    "--grace must be greater than 0."
        check dMin      "--min must be greater than 0."
        check dMax      "--max must be greater than 0."
        check dDesired  "--desired must be greater than 0."
        check dCooldown "--cooldown must be greater than 0"

        check (not $ dMin < dMax)      "--min must be less than --max."
        check (not $ dDesired >= dMin) "--desired must be greater than or equal to --min."
        check (not $ dDesired <= dMax) "--desired must be less than or equal to --max."

        check (Within dZones "abcde")      "--zones must be within [a-e]."
        check (defaultVersion == dVersion) "--version must be specified."

defineOptions "Scale" $ do
    textOption "sName" "name" ""
        "Name of the application."

    textOption "sEnv" "env" defaultEnv
        "Environment to deploy the application into."

    versionOption "sVersion" "version" defaultVersion
        "Version of the application."

    maybeIntegerOption "sGrace" "grace" 20
        "Seconds until healthchecks are activated."

    maybeIntegerOption "sMin" "min" 1
        "Minimum number of instances."

    maybeIntegerOption "sMax" "max" 20
        "Maximum number of instances."

    maybeIntegerOption "sDesired" "desired" 2
        "Desired number of instances."

    maybeIntegerOption "sCooldown" "cooldown" 60
        "Seconds between scaling activities."

deriving instance Show Scale

instance Discover Scale

instance Validate Scale where
    validate Scale{..} = do
        check sName     "--name must be specified."
        check sEnv      "--env must be specified."
        check sGrace    "--grace must be greater than 0."
        check sMin      "--min must be greater than 0."
        check sMax      "--max must be greater than 0."
        check sDesired  "--desired must be greater than 0."
        check sCooldown "--cooldown must be greater than 0"

        check (not $ sMin < sMax)      "--min must be less than --max."
        check (not $ sDesired >= sMin) "--desired must be greater than or equal to --min."
        check (not $ sDesired <= sMax) "--desired must be less than or equal to --max."

        check (defaultVersion == sVersion) "--version must be specified."

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

create :: App -> AWS ()
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
    ASG{..} = versioned aName aEnv defaultVersion

    path    = Text.unpack $ role <> ".pem"

    exist e = do
        checkError
            (("InvalidKeyPair.Duplicate" ==) . ecCode . head . eerErrors)
            (Left e)
        logInfo "KeyPair {} exists, not updating." [role]

    write k = do
        liftIO . Text.writeFile path $ ckqKeyMaterial k
        logInfo "Wrote new KeyPair to {}" [path]

deploy :: Deploy -> AWS ()
deploy d@Deploy{..} = do
    g <- findGroup name

    when (Just "Delete in progress" == join (asgStatus <$> g)) $ do
        logInfo "Waiting for previous deletion of Auto Scaling Group {}" [name]
        liftIO . threadDelay $ 10 * 1000000
        deploy d

    when (isJust g) $ throwErrorF "Auto Scaling Group {} already exists." [name]

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
        [ IpPermissionType TCP 8080 8080
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

    reg <- currentRegion

    send_ $ CreateAutoScalingGroup
        name                            -- Name
        (Members $ map (AZ reg) dZones) -- Zones
        (Just dCooldown)                -- Default Cooldown
        (Just dDesired)                 -- Desired Capacity
        (Just dGrace)                   -- Grace Period
        (Just "EC2")                    -- Health Check Type: EC2 | ELB
        name                            -- Launch Configuration Name
        (Members [])
        dMax
        dMin
        Nothing
        (Members [tag "Name" name])     -- Tags
        (Members [])
        Nothing

    logInfo "Created Auto Scaling Group {}" [name]

    -- Create and update level2 'name' DNS SRV record

    -- Health checks, monitoring, statistics
  where
    ASG{..} = versioned dName dEnv dVersion

    tag k v = Tag k
       (Just True)
       (Just name)
       (Just "auto-scaling-group")
       (Just v)

scale :: Scale -> AWS ()
scale Scale{..} = do
    AutoScalingGroup{..} <- findGroup name >>=
        noteErrorF "Auto Scaling Group %s doesn't exist." [name]

    send_ $ UpdateAutoScalingGroup
        name
        (Members asgAvailabilityZones)
        sCooldown
        sDesired
        sGrace
        Nothing
        Nothing
        sMax
        sMin
        Nothing
        (Members asgTerminationPolicies)
        Nothing

    logInfo "Updated Auto Scaling Group {}" [name]
  where
    ASG{..} = versioned sName sEnv sVersion

retire :: Cluster -> AWS ()
retire Cluster{..} = do
    send_ $ DeleteAutoScalingGroup name (Just True)
    logInfo "Delete of Auto Scaling Group {} in progress" [name]
    send_ $ DeleteLaunchConfiguration name
    logInfo "Deleted Launch Configuration {}" [name]
  where
    ASG{..} = versioned cName cEnv cVersion

info :: Cluster -> AWS ()
info Cluster{..} = return ()

route :: Cluster -> AWS ()
route Cluster{..} = return ()

findGroup :: Text -> AWS (Maybe AutoScalingGroup)
findGroup name = fmap
    (listToMaybe . members . dasgrAutoScalingGroups . dashrDescribeAutoScalingGroupsResult)
    (send $ DescribeAutoScalingGroups (Members [name]) Nothing Nothing)
