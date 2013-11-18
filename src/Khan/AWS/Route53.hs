{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Control.Concurrent  (threadDelay)
import qualified Data.Text           as Text
import           Data.Text.Format    (Shown(..))
import           Khan.Prelude        hiding (min, max)
import           Network.AWS.Route53
import           Pipes
import qualified Pipes.Prelude       as Pipes

findZoneId :: Text -> AWS HostedZoneId
findZoneId zone = do
    mz <- Pipes.find match . (paginate ~> each . lhzrHostedZones) .
        ListHostedZones Nothing $ Just 10
    hzId <$> hoistError (note msg mz)
  where
    match = (strip zone ==) . strip . hzName
    strip = Text.dropWhileEnd (== '.')
    msg   = toError $ "Unable to find a hosted zone zoned " ++ Text.unpack zone

updateRecordSet :: HostedZoneId -> ResourceRecordSet -> AWS Bool
updateRecordSet zid rset = do
    mr <- findRecordSet zid (rrsName rset) (matchRecordSet rset)
    case mr of
        Just x | x == rset -> return False
        Just x  -> modifyRecordSet zid (update x) >> return True
        Nothing -> modifyRecordSet zid create >> return True
  where
    update r = [Change DeleteAction r] ++ create
    create   = [Change CreateAction rset]

matchRecordSet rset = case rset of
      BasicRecordSet{..} -> const True
      AliasRecordSet{..} -> const True
      _                  -> (rrsSetIdentifier rset ==) . setIdentifier

setIdentifier BasicRecordSet{..} = rrsName
setIdentifier AliasRecordSet{..} = rrsName
setIdentifier s                  = rrsSetIdentifier s

modifyRecordSet :: HostedZoneId -> [Change] -> AWS ()
modifyRecordSet zid cs =
    send_ (ChangeResourceRecordSets zid $ ChangeBatch Nothing cs)
  --       >>= waitChange . crrsrChangeInfo
  -- where
  --   waitChange ChangeInfo{..} = case ciStatus of
  --       INSYNC  -> log "{} INSYNC." [show ciId]
  --       PENDING -> do
  --           log "Waiting for {}" [Shown ciId]
  --           liftIO . threadDelay $ 10 * 1000000
  --           send (GetChange ciId) >>= void . waitChange . gcrChangeInfo

findRecordSet zid name match =
    Pipes.find match . (paginate ~> each . lrrsrResourceRecordSets) $
        ListResourceRecordSets zid (Just name) Nothing Nothing Nothing

abbreviate :: Region -> Text
abbreviate NorthVirginia   = "va"
abbreviate NorthCalifornia = "ca"
abbreviate Oregon          = "or"
abbreviate Ireland         = "ie"
abbreviate Singapore       = "sg"
abbreviate Tokyo           = "tyo"
abbreviate Sydney          = "syd"
abbreviate SaoPaulo        = "sao"
