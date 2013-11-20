{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Khan.CLI.Host
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Host (commands) where

import qualified Data.Text.Encoding          as Text
import           Khan.Internal
import qualified Khan.Model.AvailabilityZone as ASG
import qualified Khan.Model.HostedZone       as HZone
import qualified Khan.Model.Image            as AMI
import qualified Khan.Model.Instance         as Instance
import qualified Khan.Model.Key              as Key
import qualified Khan.Model.LaunchConfig     as Config
import qualified Khan.Model.Profile          as Profile
import qualified Khan.Model.RecordSet        as RSet
import qualified Khan.Model.ScalingGroup     as ASG
import qualified Khan.Model.SecurityGroup    as Security
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2          hiding (ec2)
import           Network.AWS.EC2.Metadata
import           Network.AWS.Route53
import           Pipes
import qualified Pipes.Prelude            as Pipes

data Host = Host
    { hId   :: !Text
    , hFQDN :: !Text
    , hTTL  :: !Integer
    }

hostParser :: Parser Host
hostParser = Host
    <$> textOption "id" mempty
        "Instance Id."
    <*> textOption "fqdn" mempty
        "FQDN."
    <*> readOption "ttl" "SECONDS" (value 120)
        "TTL."

instance Options Host where
    discover True h@Host{..} = liftEitherT $ do
        iid  <- Text.decodeUtf8 <$> metadata InstanceId
        fqdn <- Text.decodeUtf8 <$> metadata PublicHostname
        return $! h { hId = iid, hFQDN = fqdn }
    discover False h@Host{..}
        | invalid hId || not (invalid hFQDN) = return h
        | otherwise = do
            is  <- Instance.findAll [hId] []
            dns <- noteAWS "Unable to find Public DNS for: {}" [hId] .
                join $ riitDnsName <$> listToMaybe is
            return $! h { hFQDN = dns }

    validate Host{..} =
        check (hTTL < 30) "--ttl must be greater than or equal to 30."

commands :: Mod CommandFields Command
commands = group "host" "Long long long description." $ mconcat
    [ command "register" register hostParser
        "Register an instance with DNS."
    , command "deregister" deregister hostParser
        "Deregister an instance from DNS."
    ]

-- FIXME: Handle errors retries gracefully
register :: Common -> Host -> AWS ()
register Common{..} Host{..} = do
    log "Registering host {}..." [hFQDN]
    (ns@Names{..}, ts@Tags{..}) <- describe hId
    zid  <- HZone.find tagDomain
    sets <- findPrefix zid roleName envName
    maybe (void $ create zid ns ts sets) exists $ findValue hFQDN sets
  where
    exists r = log "Record {} exists, skipping..." [rrsName r]

    create zid ns ts sets = do
        let n   = 1 + foldl' newest 0 sets
            dns = name ns ts n
            vs  = ResourceRecords [hFQDN]
            set = BasicRecordSet dns CNAME hTTL vs Nothing
        log "Creating record {} with value {}..." [dns, hFQDN]
        RSet.modify zid [Change CreateAction set]

    newest acc x = max (either (const 0) dnsOrd . parseDNS $ rrsName x) acc

    name Names{..} Tags{..} n =
        showDNS (DNS roleName tagVersion n envName $ abbreviate cRegion) tagDomain

deregister :: Common -> Host -> AWS ()
deregister _ Host{..} = do
    (_, Tags{..}) <- describe hId
    zid <- HZone.find tagDomain
    log "Searching for records for {} with value {}..." [tagRole, hFQDN]
    set <- findValue hFQDN <$> findPrefix zid tagRole tagEnv
    maybe (log_ "No record found, skipping...") (void . delete zid) set
  where
    delete zid set = do
        log "Deleting record {}..." [rrsName set]
        RSet.modify zid [Change DeleteAction set]

describe :: Text -> AWS (Names, Tags)
describe iid = do
    t@Tags{..} <- findRequiredTags iid
    return . (,t) $
        maybe (unversioned tagRole tagEnv) (versioned tagRole tagEnv) tagVersion

findPrefix :: HostedZoneId -> Text -> Text -> AWS [ResourceRecordSet]
findPrefix zid pre env = Pipes.toListM $
    RSet.findAll zid Nothing (either (const False) match . parseDNS . rrsName)
  where
    match DNS{..} = dnsRole == pre && dnsEnv == env

findValue :: Text -> [ResourceRecordSet] -> Maybe ResourceRecordSet
findValue v sets = match `find` sets
  where
    match BasicRecordSet{..} = v `elem` rrValues rrsResourceRecords
    match _                  = False
