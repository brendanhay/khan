{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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

import qualified Data.Text             as Text
import           GHC.Word
import qualified Khan.AWS.Route53      as R53
import           Khan.Internal
import           Khan.Internal.Ansible
import           Khan.Prelude          hiding (for)
import           Network.AWS
import           Network.AWS.Route53
import           Pipes
import           Text.Show.Pretty

data Record = Record
    { rZone     :: !Text
    , rName     :: !Text
    , rType     :: !RecordType
    , rValues   :: [Text]
    , rTTL      :: !Integer
    , rAlias    :: !Bool
    , rPolicy   :: !RoutingPolicy
    , rSetId    :: !Text
    , rWeight   :: !Word8
    , rFailover :: !Failover
    , rAnsible  :: !Bool
    }

-- FIXME: Get info from tags correctly
-- Ability to configure a roundrobin set or whatever and
-- create multiple records and remove unspecified

recordParser :: Parser Record
recordParser = Record
    <$> textOption "zone" mempty
        "Name of the hosted zone to modify."
    <*> textOption "name" mempty
        "Name of the record set to modify."
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
    <*> ansibleOption

        -- get zone from tag
        -- get policy from tag
        -- get set-id from tag
        -- get region from metadata
        -- get values from metadata

instance Options Record where
    validate Record{..} = do
        check rZone   "--zone must be specified."
        check rName   "--name must be specified."
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
    <*> integerOption "max" (value 4)
        "Pagination window size."
    <*> many (textOption "name" mempty
        "A name to filter by.")
    <*> many (textOption "value" mempty
        "A value to filter by.")

instance Options Search

commands :: Mod CommandFields Command
commands = group "dns" "Manage DNS Records." $ mconcat
    [ command "update" update recordParser
        "long long long description."
    , command "delete" delete recordParser
        "long long long description."
    , command "search" search searchParser
        "long long long description."
    ]

update :: Common -> Record -> AWS ()
update c@Common{..} r@Record{..}
    | not rAnsible = void upd
    | otherwise    = capture c $ do
        p <- upd
        if p
            then changed "dns record {} was updated." [rZone]
            else unchanged "dns record {} unchanged." [rZone]
  where
    upd = do
        zid <- R53.findZoneId rZone
        R53.updateRecordSet zid $ recordSet cRegion zid r

delete :: Common -> Record -> AWS ()
delete c@Common{..} r@Record{..}
    | not rAnsible = void del
    | otherwise    = capture c $ do
        p <- del
        if p
            then changed "dns record {} was removed." [rZone]
            else unchanged "dns record {} unchanged." [rZone]
  where
    del = do
        zid <- R53.findZoneId rZone
        let rset = recordSet cRegion zid r
        mr  <- R53.findRecordSet zid rName $ R53.matchRecordSet rset
        if isNothing mr
            then return False
            else do
                R53.modifyRecordSet zid [Change DeleteAction rset]
                return True

search :: Common -> Search -> AWS ()
search _ Search{..} = do
    zid <- R53.findZoneId sZone
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
    mk Basic    True  = AliasRecordSet name rType tgt Nothing

    mk Failover False = rset FailoverRecordSet rFailover
    mk Latency  False = rset LatencyRecordSet  reg
    mk Weighted False = rset WeightedRecordSet weight
    mk Basic    False = BasicRecordSet name rType rTTL rrs Nothing

    aset x y = x name rType rSetId y tgt Nothing
    rset x y = x name rType rSetId y rTTL rrs Nothing

    tgt = AliasTarget zid (head rValues) False
    rrs = ResourceRecords rValues

    weight = fromIntegral rWeight

    name = mconcat
        [ fromMaybe rName $ Text.stripSuffix rZone rName
        , "."
        , rZone
        ]
