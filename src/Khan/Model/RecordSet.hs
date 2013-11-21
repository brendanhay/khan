{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
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
    ( findAll
    , find
    , set
    , update
    , modify
    , wait
    , match
    , setId
    ) where

import           Control.Arrow
import           Control.Concurrent  (threadDelay)
import           Control.Monad
import           Data.List           (sort)
import qualified Data.Text           as Text
import           Data.Text.Format    (Shown(..))
import           Khan.Prelude        hiding (find, min, max)
import           Network.AWS.Route53 hiding (wait)
import           Pipes
import qualified Pipes.Prelude       as Pipes

findAll :: HostedZoneId
        -> (ResourceRecordSet -> Bool)
        -> Producer' ResourceRecordSet AWS ()
findAll zid p = paginate start
    >-> Pipes.map lrrsrResourceRecordSets
    >-> Pipes.concat
    >-> Pipes.filter p
  where
    start = ListResourceRecordSets zid Nothing Nothing Nothing Nothing

find :: HostedZoneId
     -> (ResourceRecordSet -> Bool)
     -> AWS (Maybe ResourceRecordSet)
find zid p = Pipes.find (const True) (findAll zid p)

set :: HostedZoneId -> Text -> [ResourceRecordSet] -> AWS Bool
set zid name rrs = do
    rrs' <- Pipes.toListM $ findAll zid (match name Nothing)

    let (cre, del) = join (***) sort $ diff rrs rrs'
        (cp,  dp)  = (null cre, null del)

    unless dp $ log "Removing {} from {}..." [f del, name]
    unless cp $ log "Adding {} to {}..." [f cre, name]

    unless (cp && dp) $ do
        void . modify zid $
            map (Change DeleteAction) del ++ map (Change CreateAction) cre
        log "Record set {} in zone {} updated." [name, unHostedZoneId zid]

    return $ any (not . null) [cre, del]
 where
   f = Text.intercalate ", " . concatMap (rrValues . rrsResourceRecords)

update :: HostedZoneId -> ResourceRecordSet -> AWS Bool
update zid rset = do
    mr <- find zid $ match (rrsName rset) (setId rset)
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

wait :: ChangeInfo -> AWS ()
wait ChangeInfo{..} = case ciStatus of
    INSYNC  -> log "{} INSYNC." [show ciId]
    PENDING -> do
        log "Waiting for {}" [Shown ciId]
        liftIO . threadDelay $ 10 * 1000000
        send (GetChange ciId) >>= void . wait . gcrChangeInfo

match :: Text -> Maybe Text -> ResourceRecordSet -> Bool
match n Nothing x = n == rrsName x
match n setid   x = match n Nothing x && setid == setId x

setId :: ResourceRecordSet -> Maybe Text
setId BasicRecordSet{..} = Nothing
setId AliasRecordSet{..} = Nothing
setId s                  = Just $ rrsSetIdentifier s
