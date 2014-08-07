{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.R53.RecordSet
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.R53.RecordSet
    ( find
    , findAll
    , set
    , update
    , modify
    , wait
    , match
    , setId
    ) where

import           Control.Arrow
import           Control.Monad
import           Data.Conduit
import qualified Data.Conduit.List    as Conduit
import           Data.List            (sort)
import qualified Data.Text            as Text
import           Khan.Internal
import           Khan.Prelude         hiding (find, min, max)
import           Network.AWS.Route53  hiding (wait)

find :: HostedZoneId
     -> (ResourceRecordSet -> Bool)
     -> AWS (Maybe ResourceRecordSet)
find zid p = findAll zid p $$ Conduit.head

findAll :: HostedZoneId
        -> (ResourceRecordSet -> Bool)
        -> Source AWS ResourceRecordSet
findAll zid p = do
    say "Searching for Record Sets in Hosted Zone {}" [zid]
    paginate (ListResourceRecordSets zid Nothing Nothing Nothing Nothing)
        $= Conduit.concatMap lrrsrResourceRecordSets
        $= Conduit.filter p

set :: HostedZoneId -> Text -> [ResourceRecordSet] -> AWS Bool
set zid name rrs = do
    rrs' <- findAll zid (match name Nothing) $$ Conduit.consume

    let (cre, del) = join (***) sort $ diff rrs rrs'
        (cp,  dp)  = (null cre, null del)

    unless dp $ say "Removing from Record Set {}:{}" [B name, values del]
    unless cp $ say "Adding to Record Set {}:{}"     [B name, values cre]

    unless (cp && dp) $ do
        void . modify zid $
            map (Change DeleteAction) del ++ map (Change CreateAction) cre
        say "Record Set {} in Hosted Zone {} updated."
            [name, unHostedZoneId zid]

    return $ any (not . null) [cre, del]
  where
    values = L . map f

    f FailoverAliasRecordSet {..} = atDNSName rrsAliasTarget
    f LatencyAliasRecordSet  {..} = atDNSName rrsAliasTarget
    f WeightedAliasRecordSet {..} = atDNSName rrsAliasTarget
    f AliasRecordSet         {..} = atDNSName rrsAliasTarget
    f x                           = Text.unwords $ rrValues (rrsResourceRecords x)

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
    INSYNC  -> say "{} INSYNC." [cid]
    PENDING -> do
        say "Waiting {} seconds for {}" [B delay, B cid]
        delaySeconds delay
        send (GetChange ciId) >>= void . wait . gcrChangeInfo
  where
    cid = unChangeId ciId

    delay = 10

match :: Text -> Maybe Text -> ResourceRecordSet -> Bool
match n Nothing x = n == rrsName x
match n setid   x = match n Nothing x && setid == setId x

setId :: ResourceRecordSet -> Maybe Text
setId BasicRecordSet{..} = Nothing
setId AliasRecordSet{..} = Nothing
setId s                  = Just $ rrsSetIdentifier s
