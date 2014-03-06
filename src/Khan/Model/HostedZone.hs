{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Model.HostedZone
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.HostedZone
    ( findId
    , findAll
    , find
    ) where

import           Data.Conduit
import qualified Data.Conduit.List   as Conduit
import qualified Data.Text           as Text
import           Khan.Internal
import           Khan.Prelude        hiding (find)
import           Network.AWS.Route53 hiding (wait)

findId :: Text -> AWS HostedZoneId
findId name =
    fmap hzId $ find name >>= noteAWS "Unable to find hosted zone: {}" [B name]

findAll :: (HostedZone -> Bool) -> Source AWS HostedZone
findAll p = paginate (ListHostedZones Nothing $ Just 10)
    $= Conduit.concatMap lhzrHostedZones
    $= Conduit.filter p

find :: Text -> AWS (Maybe HostedZone)
find name = findAll match $$ Conduit.head
  where
    match = (strip name ==) . strip . hzName
    strip = Text.dropWhileEnd (== '.')
