{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.AWS.Route53
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.AWS.Route53 where

import           Control.Applicative
import           Control.Arrow          ((***))
import           Control.Concurrent     (threadDelay)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable          (toList)
import           Data.List              ((\\), partition)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           Network.AWS.Route53
import           Pipes
import qualified Pipes.Prelude          as Pipes
import           Prelude                hiding (min, max)
import           System.Directory

findZoneId :: Text -> AWS HostedZoneId
findZoneId zone = do
    mz <- Pipes.find match . (paginate ~> each . lhzrHostedZones) .
        ListHostedZones Nothing $ Just 10
    hzId <$> noteError msg mz
  where
    match = (strip zone ==) . strip . hzName
    strip = Text.dropWhileEnd (== '.')
    msg   = "Unable to find a hosted zone zoned " ++ Text.unpack zone

findRecordSet :: HostedZoneId
              -> (ResourceRecordSet -> Bool)
              -> AWS (Maybe ResourceRecordSet)
findRecordSet zid match =
    Pipes.find match . (paginate ~> each . lrrsrResourceRecordSets) $
        ListResourceRecordSets zid Nothing Nothing Nothing Nothing

updateRecordSet zid changes = send batch >>= waitChange . crrsrChangeInfo
  where
    batch = ChangeResourceRecordSets zid $ ChangeBatch Nothing changes

    waitChange ChangeInfo{..} = case ciStatus of
        INSYNC  -> logInfo "{} INSYNC." [show ciId]
        PENDING -> do
            logInfo "Waiting for {}" [Shown ciId]
            liftIO . threadDelay $ 10 * 1000000
            send (GetChange ciId) >>= void . waitChange . gcrChangeInfo

abbreviate :: Region -> Text
abbreviate NorthVirginia   = "va"
abbreviate NorthCalifornia = "ca"
abbreviate Oregon          = "or"
abbreviate Ireland         = "ie"
abbreviate Singapore       = "sg"
abbreviate Tokyo           = "tyo"
abbreviate Sydney          = "syd"
abbreviate SaoPaulo        = "sao"
