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

module Khan.CLI.DNS (commands) where

import qualified Data.Text           as Text
import           GHC.Word
import           Khan.AWS.Route53    as R53
import           Khan.Internal
import           Khan.Prelude        hiding (for)
import           Network.AWS
import           Network.AWS.Route53
import           Pipes
import           Text.Show.Pretty

data Record = Record
    { rZone     :: !Text
    , rType     :: !RecordType
    , rValues   :: [Text]
    , rTTL      :: !Integer
    , rAlias    :: !Bool
    , rPolicy   :: !RoutingPolicy
    , rSetId    :: !Text
    , rWeight   :: !Word8
    , rFailover :: !Failover
    , rCheck    :: Maybe Text
    }

recordParser :: Parser Record
recordParser = Record
    <$> textOption "zone" mempty
        "Name of the hosted zone to modify."
    <*> readOption "type" "TYPE" (value CNAME)
        "Record set type."
    <*> many (textOption "value" mempty
        "A value to add.")
    <*> readOption "ttl" "SECONDS" (value 90)
        "Record resource cache time to live in seconds."
    <*> switchOption "alias" False
        "Whether this record should be an alias for an AWS resource."
    <*> readOption "policy" "POLICY" (value Basic)
        "Routing policy type."
    <*> textOption "set-id" (value "")
        "Differentiate and group record sets with identical policy types."
    <*> readOption "weight" "WORD8" (value 100)
        "Routing weight for the weighted policy type."
    <*> readOption "failover" "FAILOVER" (value PRIMARY)
        "Specify if this is the primary or secondary set."
    <*> optional (textOption "check" (value "")
        "Existing health check to assign.")

        -- get zone from tag
        -- get policy from tag
        -- get set-id from tag
        -- get region from metadata
        -- get values from metadata

instance Options Record where
    validate Record{..} = do
        check rZone   "--zone must be specified."
        check rValues "At least one --value must be specified."
        check (rPolicy /= Basic && Text.null rSetId)
            "--set-id must be specified for all non-basic routing policies."

data Search = Search
    { sZone   :: !Text
    , sMax    :: !Integer
    , sNames  :: [Text]
    , sValues :: [Text]
    }

searchParser :: Parser Search
searchParser = Search
    <$> textOption "zone" mempty
        "Name of the hosted zone to inspect."
    <*> readOption "max" "INT" (value 4)
        "Pagination window size."
    <*> many (textOption "name" mempty
        "A name to filter by.")
    <*> many (textOption "value" mempty
        "A value to filter by.")

instance Options Search where
    validate Search{..} =
        check sZone "--zone must be specified."

commands :: Mod CommandFields Command
commands = group "dns" "Manage DNS Records."
     $ command "create" (modify CreateAction) recordParser
        "long long long description."
    <> command "delete" (modify DeleteAction) recordParser
        "long long long description."
    <> command "search" search searchParser
        "long long long description."

modify :: ChangeAction -> Common -> Record -> AWS ()
modify act Common{..} r@Record{..} = do
    zid <- R53.findZoneId rZone
    updateRecordSet zid [Change act $ recordSet cRegion zid r]

search :: Common -> Search -> AWS ()
search _ Search{..} = do
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

recordSet :: Region -> HostedZoneId -> Record -> ResourceRecordSet
recordSet reg zid Record{..} = mk rPolicy rAlias
  where
    mk Failover True  = aset FailoverAliasRecordSet rFailover
    mk Latency  True  = aset LatencyAliasRecordSet  reg
    mk Weighted True  = aset WeightedAliasRecordSet weight
    mk Basic    True  = AliasRecordSet rZone rType tgt health

    mk Failover False = rset FailoverRecordSet rFailover
    mk Latency  False = rset LatencyRecordSet  reg
    mk Weighted False = rset WeightedRecordSet weight
    mk Basic    False = BasicRecordSet rZone rType rTTL rrs health

    aset x y = x rZone rType rSetId y tgt health
    rset x y = x rZone rType rSetId y rTTL rrs health

    tgt = AliasTarget zid (head rValues) False
    rrs = ResourceRecords rValues

    weight = fromIntegral rWeight
    health = HealthCheckId <$> rCheck
