{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Cluster
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Cluster (commands) where

import           Control.Concurrent          (threadDelay)
import           Data.Conduit
import qualified Data.Conduit.List           as Conduit
import qualified Data.HashMap.Strict         as Map
import           Data.SemVer
import qualified Data.Text                   as Text
import           Khan.Internal
import qualified Khan.Model.AvailabilityZone as AZ
import qualified Khan.Model.Image            as Image
import qualified Khan.Model.Instance         as Instance
import qualified Khan.Model.Key              as Key
import qualified Khan.Model.LaunchConfig     as Config
import           Khan.Model.Role             (Paths(..))
import qualified Khan.Model.Role             as Role
import qualified Khan.Model.ScalingGroup     as ASG
import qualified Khan.Model.SecurityGroup    as Security
import qualified Khan.Model.Tag              as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.AutoScaling     hiding (Filter)
import           Network.AWS.EC2             hiding (Filter)

data Info = Info
    { iRole :: !Role
    , iEnv  :: !Env
    }

infoParser :: EnvMap -> Parser Info
infoParser env = Info
    <$> roleOption
    <*> envOption env

instance Options Info where
    validate Info{..} =
        check iEnv "--env must be specified."

instance Naming Info where
    names Info{..} = unversioned iRole iEnv

data Deploy = Deploy
    { dRKeys    :: !RKeysBucket
    , dRole     :: !Role
    , dEnv      :: !Env
    , dDomain   :: !Text
    , dVersion  :: !Version
    , dZones    :: !String
    , dGrace    :: !Integer
    , dMin      :: !Integer
    , dMax      :: !Integer
    , dDesired  :: !Integer
    , dCooldown :: !Integer
    , dType     :: !InstanceType
    , dTrust    :: !TrustPath
    , dPolicy   :: !PolicyPath
    }

deployParser :: EnvMap -> Parser Deploy
deployParser env = Deploy
    <$> rKeysOption env
    <*> roleOption
    <*> envOption env
    <*> textOption "domain" (short 'd')
        "Instance's DNS domain."
    <*> versionOption
    <*> stringOption "zones" (value "")
         "Availability Zone suffixes the cluster will encompass."
    <*> integralOption "grace" (value 20)
        "Seconds after an auto scaling activity until healthchecks are activated."
    <*> integralOption "min" (value 1)
        "Minimum number of instances."
    <*> integralOption "max" (value 1)
        "Maximum number of instances."
    <*> integralOption "desired" (value 1)
        "Desired number of instances."
    <*> integralOption "cooldown" (value 60)
        "Seconds between subsequent auto scaling activities."
    <*> readOption "instance" "TYPE" (value M1_Medium)
        "Instance Type to provision when auto scaling occurs."
    <*> trustOption
    <*> policyOption

instance Options Deploy where
    discover _ Common{..} d@Deploy{..} = do
        zs <- AZ.getSuffixes dZones
        debug "Using Availability Zones '{}'" [zs]
        return $! d
            { dZones  = zs
            , dTrust  = pTrustPath
            , dPolicy = pPolicyPath
            }
      where
        Paths{..} = Role.paths d cConfig dTrust dPolicy

    validate Deploy{..} = do
        check dEnv   "--env must be specified."
        check dZones "--zones must be specified."

        check (dMax < dMin)     "--max must be greater than or equal to --max."
        check (dDesired < dMin) "--desired must be greater than or equal to --min."
        check (dDesired > dMax) "--desired must be less than or equal to --max."

        checkPath (_trust  dTrust)  " specified by --trust must exist."
        checkPath (_policy dPolicy) " specified by --policy must exist."

instance Naming Deploy where
    names Deploy{..} = versioned dRole dEnv dVersion

data Scale = Scale
    { sRole     :: !Role
    , sEnv      :: !Env
    , sVersion  :: !Version
    , sGrace    :: Maybe Integer
    , sMin      :: Maybe Integer
    , sMax      :: Maybe Integer
    , sDesired  :: Maybe Integer
    , sCooldown :: Maybe Integer
    }

scaleParser :: EnvMap -> Parser Scale
scaleParser env = Scale
    <$> roleOption
    <*> envOption env
    <*> versionOption
    <*> optional (integralOption "grace" mempty
        "Seconds after an auto scaling activity until healthchecks are activated.")
    <*> optional (integralOption "min" mempty
        "Minimum number of instances.")
    <*> optional (integralOption "max" mempty
        "Maximum number of instances.")
    <*> optional (integralOption "desired" mempty
        "Desired number of instances.")
    <*> optional (integralOption "cooldown" mempty
        "Seconds between subsequent auto scaling activities.")

instance Options Scale where
    validate Scale{..} = do
        check sEnv "--env must be specified."
        check (sMin >= sMax)    "--min must be less than --max."
        check (sDesired < sMin) "--desired must be greater than or equal to --min."
        check (sDesired > sMax) "--desired must be less than or equal to --max."

instance Naming Scale where
    names Scale{..} = versioned sRole sEnv sVersion

data Cluster = Cluster
    { cRole    :: !Role
    , cEnv     :: !Env
    , cVersion :: !Version
    }

clusterParser :: EnvMap -> Parser Cluster
clusterParser env = Cluster
    <$> roleOption
    <*> envOption env
    <*> versionOption

instance Options Cluster

instance Naming Cluster where
    names Cluster{..} = versioned cRole cEnv cVersion

commands :: EnvMap -> Mod CommandFields Command
commands env = group "cluster" "Auto Scaling Groups." $ mconcat
    [ command "info" info (infoParser env)
        "Display cluster information."
    , command "deploy" deploy (deployParser env)
        "Deploy a versioned cluster."
    , command "scale" scale (scaleParser env)
        "Update the scaling information for a cluster."
    , command "promote" promote (clusterParser env)
        "Promote a deployed cluster to serve traffic."
    , command "retire" retire (clusterParser env)
        "Retire a specific cluster version."
    ]

info :: Common -> Info -> AWS ()
info Common{..} Info{..} = do
    let r = _role iRole
        e = _env  iEnv

    say "Looking for Instances tagged with {} and {}" [r, e]
    is <- mapM annotate =<< Instance.findAll []
        [ Tag.filter Tag.role [r]
        , Tag.filter Tag.env  [e]
        , ec2Filter "instance-state-name" states
        , ec2Filter "tag-key" [groupTag]
        ]

    let m  = Map.fromListWith (<>) [(k, [v]) | (k, v) <- is]
        ks = Map.keys m

    if null ks
        then log_ "No Auto Scaling Groups found."
        else display m ks
  where
    annotate i@RunningInstancesItemType{..} = (,)
        <$> noteAWS "No Auto Scaling Group for: {}" [B riitInstanceId] (groupName riitTagSet)
        <*> pure i

    states = ["pending", "running", "stopping", "shutting-down"]

    groupName = Map.lookup groupTag . Tag.flatten
    groupTag  = "aws:autoscaling:groupName"

    display m ks = ASG.findAll ks $$ Conduit.mapM_ $
        \g@AutoScalingGroup{..} -> do
            ag  <- Ann g <$> Tag.lookup (Tag.flatten g)
            xs  <- noteAWS "Missing Auto Scaling Group entries: {}"
                [B asgAutoScalingGroupName]
                (Map.lookup asgAutoScalingGroupName m)

            axs <- forM xs $ \i -> Ann i <$> Tag.lookup (Tag.flatten i)

            ln >> pp (title asgAutoScalingGroupName)
            ppi 2 ag  >> ln
            ppi 2 axs >> ln

deploy :: Common -> Deploy -> AWS ()
deploy c@Common{..} d@Deploy{..} = check >> create
  where
    check = do
        g <- ASG.find d
        when (Just "Delete in progress" == join (asgStatus <$> j)) $ do
            say "Waiting for previous deletion of Auto Scaling Group {}"
                [appName]
            liftIO . threadDelay $ 10 * 1000000
            check
        when (isJust g) $
            throwAWS "Auto Scaling Group {} already exists." [B appName]

    create = do
        k <- async $ Key.create dRKeys d cLKeys
        p <- async $ Role.find d <|> Role.update d dTrust dPolicy
        s <- async $ Security.sshGroup d
        g <- async $ Security.create groupName
        a <- async $ Image.find [] [Filter "name" [imageName]]

        wait_ k
        wait_ p <* say "Found IAM Profile {}" [profileName]
        wait_ s <* say "Found SSH Group {}" [sshGroupName]
        wait_ g <* say "Found App Group {}" [groupName]

        ami <- diritImageId <$> wait a
        say "Found AMI {} named {}" [ami, imageName]

        Config.create d ami dType

        ASG.create d dDomain zones dCooldown dDesired dGrace dMin dMax

    Names{..} = names d

    zones = map (AZ cRegion) dZones

scale :: Common -> Scale -> AWS ()
scale _ s@Scale{..} = ASG.update s sCooldown sDesired sGrace sMin sMax

promote :: Common -> Cluster -> AWS ()
promote _ Cluster{..} = do
    gs <- ASG.findAll []
        $= Conduit.mapM (\g -> Ann g <$> Tag.lookup (Tag.flatten g))
        $= Conduit.filter (matchTags . annTags)
        $$ Conduit.consume

    mapM_ pp gs
  where
    matchTags Tags{..} = tagEnv == cEnv
        && _role cRole `Text.isPrefixOf` _role tagRole

--     go []  = log_ "No Auto Scaling Groups found, exiting..."
-- --    go [x] = SSH.exec x sUser key sArgs -- promote
--     go xs = do
--         forM_ xs (\(n, addr) -> log "{}) {}" [B n, P (Text.unpack addr)]) cs

--         x <- choose
--         a <- noteAWS "Invalid host selection '{}'." [x] $ x `lookup` cs
--         SSH.exec a sUser key sArgs
--       where
--         cs = map (first ) $ zip ([1..] :: [Int]) xs

--     choose = liftIO $ do
--         hSetBuffering stdout NoBuffering
--         putStr "Select the host to connect to: "
--         getLine

    -- find all role clusters in the current region

    -- sort by their weight tags

    -- present multiple choice

-- FIXME: Ensure the cluster is not currently the _only_ promoted one.
retire :: Common -> Cluster -> AWS ()
retire _ c = ASG.delete c >> Config.delete c
