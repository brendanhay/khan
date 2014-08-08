
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.Conduit
import qualified Data.Conduit.List         as Conduit
import qualified Data.List.NonEmpty        as List
import qualified Data.Text                 as Text
import           Khan.Internal
import           Khan.Model.Ansible
import qualified Khan.Model.R53.HostedZone as HZone
import qualified Khan.Model.R53.RecordSet  as RSet
import           Khan.Prelude              hiding (for)
import           Network.AWS
import           Network.AWS.Route53

data Info = Info
    { iZone :: !Text
    , iName :: Maybe Text
    }

infoParser :: Parser Info
infoParser = Info
    <$> textOption "zone" (short 'z')
        "Name of the zone to query."
    <*> optional (textOption "name" (short 'n')
        "Name of the record set to lookup.")

instance Options Info where
    discover _ _ = return . validZone iZone (\i z -> i { iZone = z })

    validate Info{..} =
        check iZone "--zone must be specified."

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

recordParser :: Parser Record
recordParser = Record
    <$> textOption "zone" (short 'z')
        "Name of the hosted zone to modify."
    <*> textOption "name" (short 'n')
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

instance Options Record where
    discover _ _ = return . validZone rZone (\r z -> r { rZone = z })

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

commands :: Mod CommandFields Command
commands = group "dns" "Manage DNS Records." $ mconcat
    [ command "info" info infoParser
        "Display information about a Record Set."
    , command "update" update recordParser
        "Ensure the Record Set exists with the specified options."
    , command "delete" delete recordParser
        "Delete an existing Record Set."
    ]

info :: Common -> Info -> AWS ()
info _ Info{..} = do
    z@HostedZone{..} <- HZone.find iZone >>=
        noteAWS "Unable to find Hosted Zone {}" [B iZone]
    say "Found Hosted Zone Id {}" [hzId]
    pPrint (overview z <-> header (Proxy :: Proxy ResourceRecordSet))
    RSet.findAll hzId (maybe (const True) (\x -> Text.isPrefixOf x . rrsName) iName)
        $$ Conduit.mapM_ (pPrint . body)
    pLn

update :: Common -> Record -> AWS ()
update c@Common{..} r@Record{..} = capture rAnsible c "dns record {}" [rName] $
    HZone.findId rZone >>= g rSet
  where
    g p zid = do
        reg <- getRegion
        if p
            then RSet.set zid (domainName rName rZone) (multiple reg zid r)
            else RSet.update zid (single reg zid r)

delete :: Common -> Record -> AWS ()
delete c@Common{..} Record{..} = capture rAnsible c "dns record {}" [rName] $ do
    zid <- HZone.findId rZone
    -- FIXME: won't work with multi value records, such as SRV
    rrs <- RSet.findAll zid (g rSet) $$ Conduit.consume
    if null rrs
        then return False
        else do
            void . RSet.modify zid $ map (Change DeleteAction) rrs
            return True
  where
    g True  = RSet.match name Nothing
    g False = (`elem` rValues) . fromMaybe "" . RSet.setId

    name = domainName rName rZone

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

validZone :: (a -> Text) -> (a -> Text -> a) -> a -> a
validZone f g x
    | invalid (f x) = x
    | otherwise     = g x ("." `suffix` f x)
  where
    suffix a b
        | a `Text.isSuffixOf` b = b
        | otherwise             = b <> a
