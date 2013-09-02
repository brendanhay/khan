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

import Control.Applicative
import Control.Error
import Khan.Options
import Options
import Network.AWS.Route53

defineOptions "RegisterDNS" $ do
    textOption "zone" "zone" ""
        "Name of the hosted zone to modify."

    textOption "domain" "domain" ""
        "Domain name of the existing or new record set."

    recordTypeOption "recordType" "type" CNAME
        "Record set type."

    textsOption "values" "value" []
        "A list of values to add."

    integerOption "ttl" "ttl" 90
        "Record resource cache time to live in seconds."

    textOption "policy" "policy" "Basic"
        "Routing policy type."

    maybeTextOption "setId" "set-id" ""
        "Differentiate and group record sets with identical policy types."

    regionOption "region" "region" Ireland
        "Region where the resource specified by the set resides."

    customOption "weight" "weight" 100 optionTypeWord8
        "Routing weight for the weighted policy type."

    failoverOption "failover" "failover" PRIMARY
        "Specify if this is the primary or secondary set."

    maybeTextOption "healthCheck" "check" ""
        "Health Check"

deriving instance Show RegisterDNS

instance Validate RegisterDNS where
    validate = return

main :: IO ()
main = runSubcommand
    [ command "register" register
    ]
  where
    register(opts :: RegisterDNS) auth = do
        scriptIO . runAWS auth $ return ()
