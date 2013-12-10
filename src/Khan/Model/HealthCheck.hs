{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.HealthCheck
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.HealthCheck where

import           Control.Monad
import           Data.Conduit
import qualified Data.Conduit.List   as Conduit
import           Khan.Prelude        hiding (find, min, max)
import           Network.AWS.Route53 hiding (wait)

findAll :: (HealthCheck -> Bool) -> Source AWS HealthCheck
findAll p = paginate (ListHealthChecks Nothing Nothing)
    $= Conduit.concatMap lhcrHealthChecks
    $= Conduit.filter p

update :: HealthCheckConfig -> AWS Bool
update hcc = do
    mhc <- findAll (match partial) $$ Conduit.head
    case mhc of
        Just x | match exact x -> return False
        Just x  -> del x >> cre >> return True
        Nothing -> cre >> return True
  where
    match xs HealthCheck{..} = all ($ hcHealthCheckConfig) xs

    exact   = [eq hccPort, eq hccResourcePath] ++ partial
    partial = [eq hccIPAddress, eq hccFullyQualifiedDomainName]

    eq f x = f hcc == f x

    cre = do
        ref <- liftIO callerRef
        send $ CreateHealthCheck ref hcc

    del = send . DeleteHealthCheck . hcId

  --   mr <- find zid (Just $ rrsName rset) (match rset)
  --   case mr of
  --       Just x | x == rset -> return False
  --       Just x  -> modify zid (upd x) >> return True
  --       Nothing -> modify zid cre >> return True
  -- where
  --   upd r = Change DeleteAction r : cre
  --   cre   = [Change CreateAction rset]

-- data HealthCheck = HealthCheck
--     { hcId                :: !HealthCheckId
--     , hcCallerReference   :: !CallerReference
--     , hcHealthCheckConfig :: !HealthCheckConfig
--     } deriving (Eq, Ord, Show, Generic)

-- data HealthCheckConfig = HealthCheckConfig
--     { hccIPAddress                :: !Text
--     , hccPort                     :: !Int
--     , hccType                     :: !Protocol
--     , hccResourcePath             :: !Text
--     , hccFullyQualifiedDomainName :: !Text
--     } deriving (Eq, Ord, Show, Generic)
