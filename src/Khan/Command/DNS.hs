{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- Module      : Khan.Command.DNS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Command.DNS where

import Control.Applicative
import Control.Monad
import Data.Vinyl
import Khan.Command.Internal
import System.Console.CmdTheLine hiding (run)

zoneName       = Field :: "zone"        ::: String
recordName     = Field :: "name"        ::: String
recordType     = Field :: "type"        ::: String
recordValue    = Field :: "value"       ::: [String]
recordTTL      = Field :: "ttl"         ::: Integer
policyType     = Field :: "policy"      ::: String
policyWeight   = Field :: "weight"      ::: String
policySet      = Field :: "set"         ::: String
policyFailover = Field :: "failover"    ::: String
policyCheck    = Field :: "healthcheck" ::: String

dns :: Command
dns = newCommand "dns" [register]

-- unregister any existing entry for this public hostname from ec2 metadata
-- and register a new one using info from tags
-- assign ()

register :: (Term (IO ()), TermInfo)
register = (go <$> run args, defTI { termName = "register" })
   where
     args = zoneName       =^$ (required . opt Nothing . optInfo . pure)
        <+> recordName     =^$ (required . opt Nothing . optInfo . pure)
        <+> recordType     =^$ (required . opt Nothing . optInfo . pure)
        <+> recordValue    =^$ (nonEmpty . optAll [] . optInfo . pure)
        <+> recordTTL      =^$ (value . opt 30 . optInfo . pure)
        <+> policyType     =^$ (required . opt Nothing . optInfo . pure)
        <+> policyWeight   =^$ (required . opt Nothing . optInfo . pure)
        <+> policySet      =^$ (required . opt Nothing . optInfo . pure)
        <+> policyFailover =^$ (required . opt Nothing . optInfo . pure)
        <+> policyCheck    =^$ (required . opt Nothing . optInfo . pure)

     go r = print r


-- unregister
