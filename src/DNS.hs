{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List              (find)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Khan.Internal
import           Network.AWS.Route53
import           Text.Show.Pretty       (ppShow)

pollDelay :: Int
pollDelay = 10 * 1000000

defineOptions "Record" $ do
    textOption "rZone" "zone" ""
        "Name of the hosted zone to modify."

    textOption "rDomain" "domain" ""
        "Domain name of the existing or new record set."

    recordTypeOption "rRecordType" "type" CNAME
        "Record set type."

    textsOption "rValues" "value" []
        "A list of values to add."

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

instance Discover Record where
    discover = return
        -- get zone from tag
        -- get domain from tag
        -- get policy from tag
        -- get set-id from tag
        -- get region from metadata
        -- get values from metadata

instance Validate Record where
    validate Record{..} = do
        check rZone   "--zone must be specified."
        check rDomain "--domain must be specified."
        check rValues "At least one --value must be specified."
        check (rPolicy /= Basic && Text.null rSetId)
            "--set-id must be specified for all non-basic routing policies."

defineOptions "Search" $ do
    textOption "sZone" "zone" ""
        "Name of the hosted zone to inspect."

    intOption "sResults" "max" 5
        "Pagination window size."

    textsOption "sNames" "name" []
        "A list of names to filter by."

    textsOption "sValues" "value" []
        "A list of values to filter by."

deriving instance Show Search

instance Discover Search

instance Validate Search where
    validate Search{..} =
        check sZone "--zone must be specified."

main :: IO ()
main = runSubcommand
    [ awsCommand "add"    $ modify CreateAction
    , awsCommand "delete" $ modify DeleteAction
    , awsCommand "search" search
    ]
  where
    modify act r@Record{..} = do
        zid <- findZoneId rZone
        chg <- send . ChangeResourceRecordSets zid $ ChangeBatch Nothing
            [ Change act $ recordSet zid r
            ]
        waitForChange $ crrsrChangeInfo chg

    search Search{..} = do
        zid <- findZoneId sZone
        listRecords zid sNames sValues sResults $ \rrs -> tryAWS $ do
            mapM_ (logInfo . ppShow) rrs
            logInfo "Press enter to continue..."
            void $ getLine

findZoneId :: Text -> AWSContext HostedZoneId
findZoneId name = do
    logInfo "Listing hosted zones..."
    hzs  <- lhzrHostedZones <$> send (ListHostedZones Nothing $ Just 100)
    hzId <$> find ((== strip name) . strip . hzName) hzs
        ?? Error ("Unable to find a hosted zone named " ++ Text.unpack name)
  where
    strip = Text.dropWhileEnd (== '.')

recordSet :: HostedZoneId -> Record -> ResourceRecordSet
recordSet zid Record{..} = mk rPolicy rAlias
  where
    mk Failover True  = aset FailoverAliasRecordSet rFailover
    mk Latency  True  = aset LatencyAliasRecordSet  rRegion
    mk Weighted True  = aset WeightedAliasRecordSet weight
    mk Basic    True  = AliasRecordSet rDomain rRecordType tgt health

    mk Failover False = rset FailoverRecordSet rFailover
    mk Latency  False = rset LatencyRecordSet  rRegion
    mk Weighted False = rset WeightedRecordSet weight
    mk Basic    False = BasicRecordSet rDomain rRecordType rTTL rrs health

    aset x y = x rDomain rRecordType rSetId y tgt health
    rset x y = x rDomain rRecordType rSetId y rTTL rrs health

    tgt = AliasTarget zid (head rValues) False
    rrs = ResourceRecords rValues

    weight = fromIntegral rWeight
    health = HealthCheckId <$> rHealthCheck

waitForChange :: ChangeInfo -> AWSContext ()
waitForChange c@ChangeInfo{..}
    | ciStatus == INSYNC = logInfo $ "Change " ++ show ciId ++ " INSYNC."
    | otherwise = do
          logStep ("Waiting for change " ++ show ciId) c
          liftIO $ threadDelay pollDelay
          send (GetChange ciId) >>= void . waitForChange . gcrChangeInfo

listRecords :: HostedZoneId
            -> [Text]
            -> [Text]
            -> Int
            -> ([ResourceRecordSet] -> AWSContext ())
            -> AWSContext ()
listRecords zid ns vs items f = cont [] $ Just initial
  where
    cont []  Nothing  = return ()
    cont rr  Nothing  = f rr
    cont rr (Just rq) = do
        res <- send rq
        let nrq = next res
            rrs = rr ++ matching res
        if length rrs < items && isJust nrq
            then cont rrs nrq
            else f (take items rrs) >> cont (drop items rrs) nrq

    initial = ListResourceRecordSets zid Nothing Nothing Nothing
        (Just $ fromIntegral items)

    next ListResourceRecordSetsResponse{..}
        | not lrrsrIsTruncated = Nothing
        | otherwise = Just $ ListResourceRecordSets zid
              lrrsrNextRecordName
              lrrsrNextRecordType
              lrrsrNextRecordIdentifier
              (Just lrrsrMaxItems)

    matching (lrrsrResourceRecordSets -> rr)
        | null ns && null vs = rr
        | otherwise          = filter (\x -> names x || values x) rr
      where
        names   = (`match` ns) . rrsName
        values  = or . map (`match` vs) . rrValues . rrsResourceRecords
        match x = or . map (\y -> y `Text.isPrefixOf` x || y `Text.isSuffixOf` x)
