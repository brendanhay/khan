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
import qualified Data.HashMap.Strict         as Map
import           Data.List                   (intersperse)
import           Data.SemVer
import qualified Data.Text                   as Text
import           Khan.Internal               hiding (infoParser)
import qualified Khan.Model.AvailabilityZone as AZ
import qualified Khan.Model.Image            as Image
import qualified Khan.Model.Instance         as Instance
import qualified Khan.Model.Key              as Key
import qualified Khan.Model.LaunchConfig     as Config
import           Khan.Model.Profile          (Policy(..))
import qualified Khan.Model.Profile          as Profile
import qualified Khan.Model.ScalingGroup     as ASG
import qualified Khan.Model.SecurityGroup    as Security
import qualified Khan.Model.Tag              as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.AutoScaling     hiding (Filter)
import           Network.AWS.EC2
import qualified Text.PrettyPrint.Boxes      as PP

data Info = Info
    { iRole :: !Role
    , iEnv  :: !Env
    }

infoParser :: EnvMap -> Parser Info
infoParser env = Info
    <$> roleOption
    <*> envOption env

instance Options Info

instance Naming Info where
    names Info{..} = unversioned iRole iEnv

data Deploy = Deploy
    { dRole     :: !Role
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
    <$> roleOption
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
        Policy{..} = Profile.policy d cConfig dTrust dPolicy

    validate Deploy{..} = do
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
    log "Looking for Instances tagged Role:{} and Env:{}"
        [_role iRole, _env iEnv]
    is <- mapM annotate =<< Instance.findAll []
        [ Tag.filter Tag.role [_role iRole]
        , Tag.filter Tag.env  [_env  iEnv]
        , Filter "tag-key" [groupTag]
        ]

    let m = Map.fromListWith (<>) [(k, [v]) | (k, v) <- is]

    log "Describing Auto Scaling Groups: [{}]" [Map.keys m]
    gs <- ASG.findAll $ Map.keys m

    log "Found {} matching Auto Scaling Groups\n" [length gs]
    forM_ gs $ \g@AutoScalingGroup{..} -> do
        xs <- noteAWS "Missing Auto Scaling Group entries: {}" [asgAutoScalingGroupName] $
            Map.lookup asgAutoScalingGroupName m
        prettyPrint $ PP g
        prettyPrint $ PP xs
  where
    annotate i@RunningInstancesItemType{..} = (,)
        <$> noteAWS "No Auto Scaling Group for: {}" [riitInstanceId] (groupName riitTagSet)
        <*> pure i

    groupName = Map.lookup groupTag . Tag.flatten
    groupTag  = "aws:autoscaling:groupName"

deploy :: Common -> Deploy -> AWS ()
deploy c@Common{..} d@Deploy{..} = do
    j <- ASG.find d

    when (Just "Delete in progress" == join (asgStatus <$> j)) $ do
        log "Waiting for previous deletion of Auto Scaling Group {}" [appName]
        liftIO . threadDelay $ 10 * 1000000
        deploy c d

    when (isJust j) $
        throwAWS "Auto Scaling Group {} already exists." [appName]

    rKeys <- Key.requireRKeys cRKeys

    k <- async $ Key.create rKeys d cLKeys
    p <- async $ Profile.find d <|> Profile.update d dTrust dPolicy
    s <- async $ Security.sshGroup d
    g <- async $ Security.create groupName
    a <- async $ Image.find [] [Filter "name" [imageName]]

    wait_ k
    wait_ p <* log "Found IAM Profile {}" [profileName]
    wait_ s <* log "Found SSH Group {}"   [sshGroupName]
    wait_ g <* log "Found App Group {}"   [groupName]

    ami <- diritImageId <$> wait a
    log "Found AMI {} named {}" [ami, imageName]

    Config.create d ami dType

    ASG.create d dDomain zones dCooldown dDesired dGrace dMin dMax
  where
    Names{..} = names d

    zones = map (AZ cRegion) dZones

scale :: Common -> Scale -> AWS ()
scale _ s@Scale{..} = ASG.update s sCooldown sDesired sGrace sMin sMax

promote :: Common -> Cluster -> AWS ()
promote _ _ = return ()

retire :: Common -> Cluster -> AWS ()
retire _ c = ASG.delete c >> Config.delete c

newtype PP a = PP a

instance Pretty (PP AutoScalingGroup) where
    pretty (PP AutoScalingGroup{..}) = PP.text name PP.// layout [hs, vs]
      where
        name = "[" ++ Text.unpack asgAutoScalingGroupName ++ "] ->"

        hs = [ "status:"
             , "zones:"
             , "cooldown:"
             , "min:"
             , "max:"
             , "desired:"
             , "created:"
             ]

        vs = [ maybe "OK" show asgStatus
             , zones
             , show asgDefaultCooldown
             , show asgMinSize
             , show asgMaxSize
             , show asgDesiredCapacity
             , formatUTC asgCreatedTime
             ]

        zones | null asgAvailabilityZones = ""
              | otherwise = (show . azRegion $ head asgAvailabilityZones)
                  ++ "["
                  ++ intersperse ',' (map azSuffix asgAvailabilityZones)
                  ++ "]"

instance Pretty (PP [RunningInstancesItemType]) where
    pretty (PP xs) = (layout . (hs :) $ map f xs) PP./+/ PP.nullBox
      where
        hs = [ "weight:"
             , "instance-id:"
             , "image-id:"
             , "public-ip:"
             , "type:"
             , "launched:"
             , "state:"
             , "reason:"
             ]

        f RunningInstancesItemType{..} =
            [ show . Tag.lookupWeight $ Tag.flatten riitTagSet
            , Text.unpack riitInstanceId
            , Text.unpack riitImageId
            , Text.unpack . fromMaybe "" $ riitIpAddress
            , show riitInstanceType
            , formatUTC riitLaunchTime
            , Text.unpack $ istName riitInstanceState
            , maybe "" show riitStateReason
            ]
