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
import Khan.Options
import Options

defineOptions "DNSOpts" $ do
    textOption "zoneName" "zone" ""
        "zoneName"

    textOption "recordName" "name" ""
        "recordName"

    recordTypeOption "recordType" "type" CNAME
        "recordType"

    textsOption "recordValues" "value" []
        "recordValue"

    integerOption "recordTTL" "ttl" 90
        "recordTTL"

    textOption "policyType" "policy" "Basic"
        "policyType"

    customOption "policyWeight" "weight" 100 optionTypeWord8
        "Routing policy weight"

    textOption "policySet" "set-id" ""
        "policySet"

    failoverOption "policyFailover" "failover" PRIMARY
        "policyFailover"

    maybeTextOption "healthCheck" "healthcheck" ""
        "Health Check"

deriving instance Show DNSOpts

main :: IO ()
main = runSubcommand
    [ command "register" register
    ]
  where
    register opts = do
        scriptIO $ print (opts :: DNSOpts)
