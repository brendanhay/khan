{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.RecordSet
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.RecordSet
    ( find
    , update
    , modify
    , wait
    , match
    ) where

import           Control.Concurrent  (threadDelay)
import           Data.Text.Format    (Shown(..))
import           Khan.Prelude        hiding (find, min, max)
import           Network.AWS.Route53 hiding (wait)
import           Pipes
import qualified Pipes.Prelude       as Pipes

find :: HostedZoneId
     -> Text
     -> (ResourceRecordSet -> Bool)
     -> AWS (Maybe ResourceRecordSet)
find zid name f = Pipes.find f . (paginate ~> each . lrrsrResourceRecordSets) $
    ListResourceRecordSets zid (Just name) Nothing Nothing Nothing

update :: HostedZoneId -> ResourceRecordSet -> AWS Bool
update zid rset = do
    mr <- find zid (rrsName rset) (match rset)
    case mr of
        Just x | x == rset -> return False
        Just x  -> modify zid (upd x) >> return True
        Nothing -> modify zid cre >> return True
  where
    upd r = Change DeleteAction r : cre
    cre   = [Change CreateAction rset]

modify :: HostedZoneId -> [Change] -> AWS ChangeInfo
modify zid cs = fmap crrsrChangeInfo . send $
    ChangeResourceRecordSets zid (ChangeBatch Nothing cs)

  --       >>= waitChange . crrsrChangeInfo
  -- where

wait :: ChangeInfo -> AWS ()
wait ChangeInfo{..} = case ciStatus of
    INSYNC  -> log "{} INSYNC." [show ciId]
    PENDING -> do
        log "Waiting for {}" [Shown ciId]
        liftIO . threadDelay $ 10 * 1000000
        send (GetChange ciId) >>= void . wait . gcrChangeInfo

match :: ResourceRecordSet -> ResourceRecordSet -> Bool
match rset = case rset of
      BasicRecordSet{..} -> const True
      AliasRecordSet{..} -> const True
      _                  -> (rrsSetIdentifier rset ==) . identifier

identifier :: ResourceRecordSet -> Text
identifier BasicRecordSet{..} = rrsName
identifier AliasRecordSet{..} = rrsName
identifier s                  = rrsSetIdentifier s
