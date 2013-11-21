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

import qualified Data.List.NonEmpty    as List
import qualified Data.Text             as Text
import           GHC.Word
import           Khan.Internal
import           Khan.Internal.Ansible
import           Khan.Internal.Text
import qualified Khan.Model.HostedZone as HZone
import qualified Khan.Model.RecordSet  as RSet
import           Khan.Prelude          hiding (for)
import           Network.AWS
import           Network.AWS.Route53
import           Pipes
import qualified Pipes.Prelude         as Pipes
import           Text.Show.Pretty

data Record = Record
    { rZone    :: !Text
    , rName    :: !Text
    , rType    :: !RecordType
    , rValues  :: NonEmpty Text
    , rTTL     :: !Integer
    , rAlias   :: !Bool
    , rPolicy  :: !RoutingPolicy
    , rWeight  :: !Integer
    , rFail    :: !Failover
    , rSet     :: !Bool
    , rAnsible :: !Bool
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
    <*> (List.fromList <$> some (textOption "value" mempty
        "A value to add."))
    <*> readOption "ttl" "SECONDS" (value 90)
        "Record resource cache time to live in seconds."
    <*> switchOption "alias" False
        "Whether this record should be an alias for an AWS resource."
    <*> readOption "policy" "POLICY" (value Basic)
        "Routing policy type."
    <*> readOption "weight" "WORD8" (value 100)
        "Routing weight for the weighted policy type."
    <*> readOption "failover" "FAILOVER" (value PRIMARY)
        "Specify if this is the primary or secondary set."
    <*> switchOption "set" False
        "Use the specified values for multiple individual records sets."
    <*> ansibleOption

        -- get zone from tag
        -- get policy from tag
        -- get region from metadata
        -- get values from metadata

instance Options Record where
    discover _ r@Record{..}
        | invalid rZone = return r
        | otherwise     = return $ r { rZone = tappend rZone "." }

    validate Record{..} = do
        check rZone   "--zone must be specified."
        check rName   "--name must be specified."
        check rValues "--value must be specified."

        check (not $ "." `Text.isSuffixOf` rZone)
            "--zone must be suffixed with '.'."

        case rType of
            SRV -> return ()
            _   -> check (not rSet && List.length rValues /= 1)
                "Exactly one --value must be specified if --set is not used."

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
    | rAnsible  = capture c "dns record {}" [rName] f
    | otherwise = void f
  where
    f = HZone.find rZone >>= g rSet

    g True  zid = RSet.set zid (domainName rName rZone) (multiple cRegion zid r)
    g False zid = RSet.update zid (single cRegion zid r)

delete :: Common -> Record -> AWS ()
delete c@Common{..} r@Record{..}
    | rAnsible  = capture c "dns record {}" [rName] f
    | otherwise = void f
  where
    f = do
        zid <- HZone.find rZone
        -- FIXME: won't work with multi value records, such as SRV
        rrs <- Pipes.toListM $ RSet.findAll zid (g rSet)
        if null rrs
            then return False
            else do
                void . RSet.modify zid $ map (Change DeleteAction) rrs
                return True

    g True  = RSet.match name Nothing
    g False = (`elem` rValues) . fromMaybe "" . RSet.setId

    name = domainName rName rZone

search :: Common -> Search -> AWS ()
search _ Search{..} = do
    zid <- HZone.find sZone
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

single :: Region -> HostedZoneId -> Record -> ResourceRecordSet
single reg zid r@Record{..} = recordSet reg zid r $ List.sort rValues

multiple :: Region -> HostedZoneId -> Record -> [ResourceRecordSet]
multiple reg zid r@Record{..} = List.toList $
    List.map (recordSet reg zid r . (:|[])) rValues

recordSet :: Region
          -> HostedZoneId
          -> Record
          -> NonEmpty Text
          -> ResourceRecordSet
recordSet reg zid Record{..} vs = mk rPolicy rAlias
  where
    mk Failover True  = aset FailoverAliasRecordSet rFail
    mk Latency  True  = aset LatencyAliasRecordSet reg
    mk Weighted True  = aset WeightedAliasRecordSet rWeight
    mk Basic    True  = AliasRecordSet name rType tgt Nothing

    mk Failover False = rset FailoverRecordSet rFail
    mk Latency  False = rset LatencyRecordSet reg
    mk Weighted False = rset WeightedRecordSet rWeight
    mk Basic    False = BasicRecordSet name rType rTTL rrs Nothing

    aset ctor x = ctor name rType (List.head vs) x tgt Nothing
    rset ctor x = ctor name rType (List.head vs) x rTTL rrs Nothing

    name = domainName rName rZone

    tgt = AliasTarget zid (List.head vs) False
    rrs = ResourceRecords (List.toList vs)

domainName :: Text -> Text -> Text
domainName name zone = name <> "." <> zone
