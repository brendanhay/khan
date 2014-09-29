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
import           Data.Conduit
import qualified Data.Conduit.List                   as Conduit
import qualified Data.HashMap.Strict                 as Map
import           Data.List                           (partition)
import           Data.SemVer
import           Khan.Internal
import qualified Khan.Model.AutoScaling.LaunchConfig as Config
import qualified Khan.Model.AutoScaling.ScalingGroup as ASG
import qualified Khan.Model.EC2.AvailabilityZone     as AZ
import qualified Khan.Model.EC2.Image                as Image
import qualified Khan.Model.EC2.Instance             as Instance
import qualified Khan.Model.EC2.SecurityGroup        as Security
import qualified Khan.Model.ELB.LoadBalancer         as Balancer
import           Khan.Model.ELB.Types                as Balancer
import           Khan.Model.IAM.Role                 (Paths(..))
import qualified Khan.Model.IAM.Role                 as Role
import qualified Khan.Model.IAM.ServerCertificate    as Cert
import qualified Khan.Model.Key                      as Key
import qualified Khan.Model.R53.HostedZone           as HZone
import qualified Khan.Model.R53.RecordSet            as RSet
import qualified Khan.Model.Tag                      as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.AutoScaling             hiding (Filter)
import           Network.AWS.EC2                     hiding (Filter)
import           Network.AWS.ELB
import           Network.AWS.Route53                 hiding (Protocol(..))

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
    { dRKeys        :: !RKeysBucket
    , dRole         :: !Role
    , dEnv          :: !Env
    , dDomain       :: !Text
    , dVersion      :: !Version
    , dZones        :: [Char]
    , dGrace        :: !Integer
    , dMin          :: !Integer
    , dMax          :: !Integer
    , dDesired      :: !Integer
    , dCooldown     :: !Integer
    , dType         :: !InstanceType
    , dTrust        :: !TrustPath
    , dPolicy       :: !PolicyPath
    , dBalance      :: [Balancer.Mapping]
    , dAutoPromote  :: !Bool
    , dAutoRetire   :: !Bool
    , dPromoteDelay :: !Int
    , dRetireDelay  :: !Int
    }

deployParser :: EnvMap -> Parser Deploy
deployParser env = Deploy
    <$> rKeysOption env
    <*> roleOption
    <*> envOption env
    <*> textOption "domain" (short 'd')
        "Instance's DNS domain."
    <*> versionOption
    <*> stringOption "zones" (value [])
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
    <*> instanceOption
    <*> trustOption
    <*> policyOption
    <*> many (customOption "elb" "ELB" parseString mempty
        "Balancer shizzle (e.g. https:443-http:8080/status)")
    <*> switchOption "auto-promote" False
        "Automatically promote the new cluster."
    <*> switchOption "auto-retire" False
        "Automatically retire old clusters."
    <*> integralOption "auto-promote-delay" (value 0)
        "Delay (seconds) before auto-promoting the new cluster."
    <*> integralOption "auto-retire-delay" (value 0)
        "Delay (seconds) before auto-retiring the old clusters."

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

        check (not dAutoPromote && dAutoRetire) "--auto-retire requires --auto-promote"

        checkFile (_trust  dTrust)  " specified by --trust must exist."
        checkFile (_policy dPolicy) " specified by --policy must exist."

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
    when iAll $ do
        say  "Searching for Images tagged with {}" [iRole]
        mapM_ (pPrint . overview) =<< Image.findAll []
            [ Tag.filter Tag.role [_role iRole]
            ]

    say "Searching for Instances tagged with {} and {}" [B iRole, B iEnv]
    ms <- Instance.findAll []
        [ Tag.filter Tag.role [_role iRole]
        , Tag.filter Tag.env  [_env  iEnv]
        , ec2Filter "instance-state-name" states
        , ec2Filter "tag-key" [Tag.group]
        ]

    let is = mapMaybe (fmap unwrap . Tag.annotate) ms
        m  = Map.fromListWith (<>) [(k, [v]) | (Just k, v) <- is]

    if null (Map.keys m)
        then log_ "No Auto Scaling Groups found."
        else groups m >> pLn
  where
    states = ["pending", "running", "stopping", "shutting-down"]
    unwrap = (tagGroup . annTags *** annValue) . join (,)

    groups m = ASG.findAll (Map.keys m)
        $= Conduit.mapMaybe Tag.annotate
        $$ Conduit.mapM_ $ \ag -> do
            let asg@AutoScalingGroup{..} = annValue ag

            is <- mapMaybe Tag.annotate <$>
                noteAWS "Missing Auto Scaling Group entries: {}"
                    [B asgAutoScalingGroupName]
                    (Map.lookup asgAutoScalingGroupName m)

            pPrint (overview ag)

            if null asgLoadBalancerNames
                then log_ "No associated Elastic Load Balancers found."
                else do
                    bs <- Balancer.findAll (Balancer.balancerNamesFromASG asg) $$ Conduit.consume
                    pPrint $ header (Proxy :: Proxy LoadBalancerDescription)
                         <-> body bs

            if null is
                then log_ "No associated Auto Scaling Instances found."
                else pPrint $ header (Proxy :: Proxy RunningInstancesItemType)
                          <-> body is

deploy :: Common -> Deploy -> AWS ()
deploy c@Common{..} d@Deploy{..} = ensure >> create >> autoPromote >> autoRetire
  where
    ensure = do
        g <- ASG.find d
        when (Just "Delete in progress" == join (asgStatus <$> g)) $ do
            say "Waiting for previous deletion of Auto Scaling Group {}"
                [appName]
            delaySeconds 10
            ensure

    create = do
        k <- async $ Key.create dRKeys d cLKeys
        r <- async $ Role.find d <|> Role.update d dTrust dPolicy
        g <- async $ Security.createDefaults d
        i <- async $ Image.find [] [ec2Filter "name" [imageName]]

        wait_ k
        wait_ r <* say "Found IAM Profile {}" [profileName]
        wait_ g

        ami <- diritImageId <$> wait i
        say "Found AMI {} named {}" [ami, imageName]

        unless (null dBalance) balance

        Config.create d ami dType
        ASG.create d balancers dDomain zones dCooldown dDesired dGrace dMin dMax

    balance = do
        ac <- async $ Cert.find dDomain
        ab <- async $ mapM (Balancer.find . Balancer.mkBalancerName d) dBalance

        ct <- wait ac >>= noteAWS "Missing Server Certificate for {}" [B dDomain]
        bs <- wait ab

        forM_ (dBalance `zip` map isJust bs) $ \(b, exists) ->
            if exists
                then say "Load Balancer {} already exists." [B $ Balancer.mkBalancerName d b]
                else Balancer.create d zones b ct

    autoPromote = when dAutoPromote $ do
        say "Waiting {} seconds before auto-promoting" [dPromoteDelay]
        delaySeconds dPromoteDelay
        say "Auto-promoting cluster {} at version {} in {}" [B dRole, B dVersion, B dEnv]
        promote c $ Cluster dRole dEnv dVersion

    autoRetire = when dAutoRetire $ do
        say "Waiting {} seconds before auto-retiring" [dRetireDelay]
        vs <- ASG.findAll []
            $= Conduit.mapMaybe Tag.annotate
            $= Conduit.filter (matchTags . annTags)
            $= Conduit.mapMaybe (tagVersion . annTags)
            $$ Conduit.consume
        delaySeconds dPromoteDelay
        forM_ vs $ \v -> do
            say "Auto-retiring cluster {} at version {} in {}" [B dRole, B v, B dEnv]
            retire c $ Cluster dRole dEnv v

    matchTags Tags{..} = tagEnv == dEnv && dRole == tagRole && Just dVersion /= tagVersion

    Names{..} = names d
    balancers = map (Balancer.mkBalancerName d) dBalance
    zones     = map (AZ cRegion) dZones

promote :: Common -> Cluster -> AWS ()
promote _ c@Cluster{..} = do
    gs <- ASG.findAll []
        $= Conduit.mapMaybe Tag.annotate
        $= Conduit.filter (matchTags . annTags)
        $$ Conduit.consume

    (next, prev) <- targets gs

    let name = asgAutoScalingGroupName (annValue next)

    promote' name next
    rebalance next

    if null prev
        then log_ "No previous Group or Instances to demote."
        else demote prev

    say "Successfully promoted {}" [name]
  where
    matchTags Tags{..} = tagEnv == cEnv && cRole == tagRole

    targets gs
        | (x:xs, ys) <- partition ((Just cVersion ==) . tagVersion . annTags) gs
            = return (x, xs ++ ys)
        | otherwise
            = throwAWS "Unable to find Auto Scaling Group Version {}." [cVersion]

    rebalance next = do
        let dom = tagDomain $ annTags next
        let elbs = Balancer.balancerNamesFromASG (annValue next)
        if null elbs
            then log_ "No associated Elastic Load Balancers found."
            else do
                mb <- Balancer.findAll elbs $$ Conduit.consume
                forM_ mb $ \lbd -> do
                    tgt <- noteAWS "Load Balancer {} doesn't contain a DNS entry."
                        [lbdLoadBalancerName lbd] (lbdDNSName lbd)
                    cid <- noteAWS "Load Balancer {} doesn't contain a Hosted Zone."
                        [lbdLoadBalancerName lbd] (lbdCanonicalHostedZoneNameID lbd)
                    zid <- HZone.findId dom
                    dns <- Balancer.fqdn c dom lbd
                    say "Assigning Record Set {} to {}" [dns, tgt]
                    void $ RSet.update zid
                        (AliasRecordSet dns A (AliasTarget (hostedZoneId cid) tgt False) Nothing)

    promote' name next = do
        say "Searching for running Instances tagged with {}" [name]
        is <- map riitInstanceId <$>
            Instance.findAll [] [ Tag.filter Tag.group [name]
                                , ec2Filter "instance-state-name" ["running"]
                                ]

        say "Promoting Auto Scaling Group {}" [name]
        ag <- sendAsync $ CreateOrUpdateTags (Members [reweight promoted next])

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
            say "Searching for Instances tagged with {}" [asgAutoScalingGroupName]
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

retire :: Common -> Cluster -> AWS ()
retire _ c@Cluster{..} = do
    ag <- ASG.find c >>= noteAWS "Unable to find Auto Scaling Group {}" [appName]
        . join
        . fmap Tag.annotate
    dg <- async $ ASG.delete c >> Config.delete c
    let elbs = Balancer.balancerNamesFromASG (annValue ag)
    ab <- if null elbs
        then log_ "No associated Elastic Load Balancers found." >> return []
        else Balancer.findAll elbs $$ Conduit.consume
    db <- async $ forM_ ab $ traverse_ Balancer.delete . Balancer.balancerNameFromDescription
    forM_ ab $ \lbd -> do
        let dom = tagDomain (annTags ag)
        dns <- Balancer.fqdn c dom lbd
        zid <- HZone.findId dom
        mr  <- RSet.find zid (match (lbdDNSName lbd) dns)
        for_ mr $ \r -> do
            say "Deleting Record Set {} from Hosted Zone {}"
                [dns, unHostedZoneId zid]
            void $ RSet.modify zid [Change DeleteAction r]
    wait_ dg
    wait_ db
  where
    match lb dns AliasRecordSet{..} = Just dns `cmp` Just rrsName
        && lb `cmp` Just (atDNSName rrsAliasTarget)
    match _ _ _ = False

    cmp a b = (stripText "." <$> a) == (stripText "." <$> b)

    Names{..} = names c
