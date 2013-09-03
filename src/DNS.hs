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

import Control.Error
import Control.Monad.IO.Class
import Khan.Internal
import Network.AWS.Route53

defineOptions "Register" $ do
    textOption "rZone" "zone" ""
        "Name of the hosted zone to modify."

    textOption "rDomain" "domain" ""
        "Domain name of the existing or new record set."

    recordTypeOption "rRecordType" "type" CNAME
        "Record set type."

    textsOption "rValues" "value" []
        "A list of values to add."

    integerOption "rTtl" "ttl" 90
        "Record resource cache time to live in seconds."

    boolOption "rAlias" "alias" False
        "Whether this record should be an alias for an AWS resource."

    routingPolicyOption "rPolicy" "policy" Basic
        "Routing policy type."

    maybeTextOption "rSetId" "set-id" ""
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
    register r@Register{..} aws = fmapLT show $ do
        logStep "Running DNS Register..." r
        aws $ do
            ListHostedZonesResponse{..} <- send (ListHostedZones Nothing $ Just 1000)
            liftIO $ print lhzrHostedZones

    unregister u@Unregister{..} aws = fmapLT show $ do
        logStep "Running DNS Unregister..." u
        aws $ return ()
