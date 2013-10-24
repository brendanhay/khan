{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.CLI.DNS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.DNS (cli) where

import qualified Data.Text           as Text
import           Khan.AWS.Route53    as R53
import           Khan.Internal
import           Khan.Prelude        hiding (for)
import           Network.AWS
import           Network.AWS.Route53
import           Pipes
import           Text.Show.Pretty

defineOptions "Record" $ do
    textOption "rZone" "zone" ""
        "Name of the hosted zone to modify."

    recordTypeOption "rRecordType" "type" CNAME
        "Record set type."

    textsOption "rValues" "value" []
        "A value to add."

    integerOption "rTTL" "ttl" 90
        "Record resource cache time to live in seconds."

    boolOption "rAlias" "alias" False
        "Whether this record should be an alias for an AWS resource."

    routingPolicyOption "rPolicy" "policy" Basic
        "Routing policy type."

    textOption "rSetId" "set-id" ""
        "Differentiate and group record sets with identical policy types."

    regionOption "rRegion" "region" Ireland
        "Region to use for regionalised routing records."

    customOption "rWeight" "weight" 100 optionTypeWord8
        "Routing weight for the weighted policy type."

    failoverOption "rFailover" "failover" PRIMARY
        "Specify if this is the primary or secondary set."

    maybeTextOption "rHealthCheck" "check" ""
        "Existing health check to assign."

deriving instance Show Record

instance Discover Record
--    discover = return
        -- get zone from tag
        -- get policy from tag
        -- get set-id from tag
        -- get region from metadata
        -- get values from metadata

instance Validate Record where
    validate Record{..} = do
        check rZone   "--zone must be specified."
        check rValues "At least one --value must be specified."
        check (rPolicy /= Basic && Text.null rSetId)
            "--set-id must be specified for all non-basic routing policies."

defineOptions "Search" $ do
    textOption "sZone" "zone" ""
        "Name of the hosted zone to inspect."

    integerOption "sMax" "max" 4
        "Pagination window size."

    textsOption "sNames" "name" []
        "A name to filter by."

    textsOption "sValues" "value" []
        "A value to filter by."

deriving instance Show Search

instance Discover Search

instance Validate Search where
    validate Search{..} =
        check sZone "--zone must be specified."

cli :: Command
cli = Command "dns" "Manage DNS Records."
    [ subCommand "create" $ modify CreateAction
    , subCommand "delete" $ modify DeleteAction
    , subCommand "search" search
    ]

modify :: ChangeAction -> Record -> AWS ()
modify act r@Record{..} = do
    zid <- R53.findZoneId rZone
    updateRecordSet zid [Change act $ recordSet zid r]

search :: Search -> AWS ()
search Search{..} = do
    zid <- findZoneId sZone
    runEffect $ for (paginate $ start zid) (liftIO . display)
  where
    display (matching -> rrs) = unless (null rrs) $ do
        mapM_ (putStrLn . ppShow) rrs
        log_ "Press enter to continue..." >> void getLine

    start zid = ListResourceRecordSets zid Nothing Nothing Nothing (Just sMax)

    matching (lrrsrResourceRecordSets -> rr)
        | null sNames && null sValues = rr
        | otherwise = filter (\x -> ns x || vs x) rr

    ns = (`match` sNames) . rrsName
    vs = any (`match` sValues) . rrValues . rrsResourceRecords

    match x = any (\y -> y `Text.isPrefixOf` x || y `Text.isSuffixOf` x)

recordSet :: HostedZoneId -> Record -> ResourceRecordSet
recordSet zid Record{..} = mk rPolicy rAlias
  where
    mk Failover True  = aset FailoverAliasRecordSet rFailover
    mk Latency  True  = aset LatencyAliasRecordSet  rRegion
    mk Weighted True  = aset WeightedAliasRecordSet weight
    mk Basic    True  = AliasRecordSet rZone rRecordType tgt health

    mk Failover False = rset FailoverRecordSet rFailover
    mk Latency  False = rset LatencyRecordSet  rRegion
    mk Weighted False = rset WeightedRecordSet weight
    mk Basic    False = BasicRecordSet rZone rRecordType rTTL rrs health

    aset x y = x rZone rRecordType rSetId y tgt health
    rset x y = x rZone rRecordType rSetId y rTTL rrs health

    tgt = AliasTarget zid (head rValues) False
    rrs = ResourceRecords rValues

    weight = fromIntegral rWeight
    health = HealthCheckId <$> rHealthCheck
