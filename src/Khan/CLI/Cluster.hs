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


import           Control.Arrow
import           Control.Concurrent          (threadDelay)
import           Data.Conduit
import qualified Data.Conduit.List           as Conduit
import qualified Data.HashMap.Strict         as Map
import           Data.List                   (partition)
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
    , iAll  :: !Bool
    }

infoParser :: EnvMap -> Parser Info
infoParser env = Info
    <$> roleOption
    <*> envOption env
    <*> switchOption "all" False
        "Show all available versions, even if they are not deployed."

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
    , command "promote" promote (clusterParser env)
        "Promote a deployed cluster to serve traffic."
    , command "scale" scale (scaleParser env)
        "Update the scaling information for a cluster."
    , command "retire" retire (clusterParser env)
        "Retire a specific cluster version."
    ]

info :: Common -> Info -> AWS ()
info Common{..} Info{..} = do
    let r  = _role iRole
        e  = _env  iEnv

    when iAll (images r)

    say "Looking for Instances tagged with {} and {}" [r, e]
    is <- mapM (fmap unwrap . Tag.annotate) =<< Instance.findAll []
        [ Tag.filter Tag.role [r]
        , Tag.filter Tag.env  [e]
        , ec2Filter "instance-state-name" states
        , ec2Filter "tag-key" [Tag.group]
        ]

    let m  = Map.fromListWith (<>) [(k, [v]) | (Just k, v) <- is]
        ks = Map.keys m

    if null ks
        then log_ "No Auto Scaling Groups found."
        else instances m ks >> pLn
  where
    states = ["pending", "running", "stopping", "shutting-down"]

    unwrap = (tagGroup . annTags *** annValue) . join (,)

    images r = do
        say  "Looking for Images tagged with {}" [r]
        mapM_ (pPrint . overview) =<< Image.findAll []
            [ Tag.filter Tag.role [r]
            ]

    instances m ks = ASG.findAll ks $$ Conduit.mapM_ $
        \g@AutoScalingGroup{..} -> do
            ag <- Tag.annotate g
            xs <- mapM Tag.annotate =<<
                noteAWS "Missing Auto Scaling Group entries: {}"
                    [B asgAutoScalingGroupName]
                    (Map.lookup asgAutoScalingGroupName m)
            pPrint (overview ag)
            if null xs
               then log_ "No Auto Scaling Instances found."
               else pPrint $ header (Proxy :: Proxy RunningInstancesItemType)
                         <-> body xs

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

promote :: Common -> Cluster -> AWS ()
promote _ c@Cluster{..} = do
    gs <- ASG.findAll []
        $= Conduit.mapM Tag.annotate
        $= Conduit.filter (matchTags . annTags)
        $$ Conduit.consume

    (next, prev) <- targets gs

    let name = asgAutoScalingGroupName (annValue next)

    promote' name next

    if null prev
        then log_ "No previous Group or Instances to demote."
        else demote prev

    say "Successfully promoted {}" [name]
  where
    matchTags Tags{..} = tagEnv == cEnv
        && _role cRole `Text.isPrefixOf` _role tagRole

    targets gs
        | (x:xs, ys) <- partition ((Just cVersion ==) . tagVersion . annTags) gs
            = return (x, xs ++ ys)
        | otherwise
            = throwAWS "Unable to find Auto Scaling Group Version {}." [cVersion]

    promote' name next = do
        say "Looking for Instances tagged with {}" [name]
        is <- map riitInstanceId <$>
            Instance.findAll [] [Tag.filter Tag.group [name]]

        say "Promoting Auto Scaling Group {}" [name]
        ag <- sendAsync . CreateOrUpdateTags $ Members [reweight promoted next]

        say "Promoting Instances: {}" [L is]
        ai <- sendAsync $ CreateTags is [ResourceTagSetItemType Tag.weight promoted]

        wait_ ag
        wait_ ai

    demote prev = do
        say "Demoting Auto Scaling Groups: {}"
            [L $ map (asgAutoScalingGroupName . annValue) prev]
        ag <- sendAsync . CreateOrUpdateTags $
            Members (map (reweight demoted) prev)

        as <- forM prev $ \(Ann AutoScalingGroup{..} _) -> async $ do
            say "Looking for Instances tagged with {}" [asgAutoScalingGroupName]
            is <- map riitInstanceId <$> Instance.findAll []
                [ Tag.filter Tag.group [asgAutoScalingGroupName]
                ]

            say "Demoting Instances: {}" [L is]
            send_ $ CreateTags is [ResourceTagSetItemType Tag.weight demoted]

        wait_ ag
        mapM_ wait_ as

    reweight w a = ASG.tag (asgAutoScalingGroupName $ annValue a) Tag.weight w

    promoted = "100"
    demoted  = "0"

    Names{..} = names c

scale :: Common -> Scale -> AWS ()
scale _ s@Scale{..} = ASG.update s sCooldown sDesired sGrace sMin sMax

-- FIXME: Ensure the cluster is not currently the _only_ promoted one.
retire :: Common -> Cluster -> AWS ()
retire _ c = ASG.delete c >> Config.delete c
