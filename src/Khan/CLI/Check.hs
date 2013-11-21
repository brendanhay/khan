{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Check
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Check (commands) where

import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.HealthCheck as Check
import           Khan.Prelude           hiding (for)
import           Network.AWS.Route53

data Check = Check
    { cZone    :: !Text
    , cIP      :: !Text
    , cPort    :: !Integer
    , cProto   :: !Protocol
    , cPath    :: !Text
    , cAnsible :: !Bool
    }

checkParser :: Parser Check
checkParser = Check
    <$> textOption "zone" mempty
        "Name of the hosted zone to associated the health check with."
    <*> textOption "address" mempty
        "IP Address of the endpoint to perform health checks on."
    <*> integerOption "port" (value 80)
        "Port of the endpoint to perform health checks on."
    <*> readOption "protocol" "PROTO" (value HTTP)
        "Protocol to communicate with the endpoint."
    <*> textOption "path" (value "/")
        "Path to request when the health check is invoked."
    <*> ansibleOption

instance Options Check

commands :: Mod CommandFields Command
commands = group "check" "Manage DNS Health Checks." $ mconcat
    [ command "update" update checkParser
        "long long long description."
    ]

update :: Common -> Check -> AWS ()
update c@Common{..} Check{..}
    | not cAnsible = void f
    | otherwise    = capture c "health check {}:{}{}" ps f
  where
    ps = [show cIP, show cPort, show cPath]

    f = Check.update $
        HealthCheckConfig cIP (fromIntegral cPort) cProto cPath cZone
