{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

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
import           Control.Error
import           Data.List           (find)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Khan.Internal
import           Network.AWS.Route53

defineOptions "Register" $ do
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

deriving instance Show Register

instance Discover Register where
    discover = return
        -- get zone from tag
        -- get domain from tag
        -- get policy from tag
        -- get set-id from tag
        -- get region from metadata
        -- get values from metadata

instance Validate Register where
    validate Register{..} = do
        check rZone   "--zone must be specified."
        check rDomain "--domain must be specified."
        check rValues "At least one --value must be specified."
        check (rPolicy /= Basic && Text.null rSetId)
            "--set-id must be specified for all non-basic routing policies."

defineOptions "Unregister" $ do
    textOption "uZone" "zone" ""
        "Name of the hosted zone to modify."

    textsOption "uValues" "value" []
        "A list of values to remove from matching records."

deriving instance Show Unregister

instance Discover Unregister where
    discover = return
        -- get zone from tag
        -- get values from tag + metadata

instance Validate Unregister where
    validate Unregister{..} = do
        check uZone   "--zone must be specified."
        check uValues "At least one --value must be specified."

main :: IO ()
main = runSubcommand
    [ command "register"   register
    , command "unregister" unregister
    ]
  where
    register :: Register -> (AWSContext () -> Script ()) -> Script ()
    register r@Register{..} aws = do
        logStep "Running DNS Register..." r
        aws $ do
            logInfo "Listing hosted zones..."
            hzs <- lhzrHostedZones <$> send (ListHostedZones Nothing $ Just 100)
            hz  <- findZone rZone hzs
            hzi <- Text.stripPrefix "/hostedzone/" (hzId hz)
                ?? Error ("Invalid hosted zone identifier: " ++ Text.unpack (hzId hz))
            logStep ("Found hosted zone: " ++ Text.unpack hzi) hz
            res <- send . ChangeResourceRecordSets hzi $ ChangeBatch Nothing
                [ Change CreateAction $ mkRRSet
                    hzi rPolicy rDomain rRecordType rSetId rAlias rTTL
                    (fromIntegral rWeight) rFailover rRegion rHealthCheck rValues
                ]

            logInfo $ show res

    unregister u@Unregister{..} aws = do
        logStep "Running DNS Unregister..." u
        aws $ return ()

findZone :: Text -> [HostedZone] -> AWSContext HostedZone
findZone name hzs = find ((== strip name) . strip . hzName) hzs
    ?? Error ("Unable to find a hosted zone named " ++ Text.unpack name)
  where
    strip = Text.dropWhileEnd (== '.')

mkRRSet :: Text
        -> RoutingPolicy
        -> Text
        -> RecordType
        -> Text
        -> Bool
        -> Integer
        -> Integer
        -> Failover
        -> Region
        -> Maybe Text
        -> [Text]
        -> ResourceRecordSet
mkRRSet zone policy name typ setid alias ttl weight failover region health vs =
    mk policy alias
  where
    mk Failover True  = aset FailoverAliasRecordSet $ failover
    mk Latency  True  = aset LatencyAliasRecordSet  $ region
    mk Weighted True  = aset WeightedAliasRecordSet $ weight
    mk Basic    True  = AliasRecordSet name typ tgt health

    mk Failover False = rset FailoverRecordSet $ failover
    mk Latency  False = rset LatencyRecordSet  $ region
    mk Weighted False = rset WeightedRecordSet $ weight
    mk Basic    False = BasicRecordSet name typ ttl rrs health

    aset x y = x name typ setid y tgt health
    rset x y = x name typ setid y ttl rrs health

    tgt = AliasTarget zone (head vs) False
    rrs = ResourceRecords vs
