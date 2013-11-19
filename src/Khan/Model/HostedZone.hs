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
    ( find
    ) where

import qualified Data.Text           as Text
import           Khan.Prelude        hiding (find)
import           Network.AWS.Route53 hiding (wait)
import           Pipes
import qualified Pipes.Prelude       as Pipes

find :: Text -> AWS HostedZoneId
find zone = do
    mz <- Pipes.find match . (paginate ~> each . lhzrHostedZones) .
        ListHostedZones Nothing $ Just 10
    hzId <$> hoistError (note msg mz)
  where
    match = (strip zone ==) . strip . hzName
    strip = Text.dropWhileEnd (== '.')
    msg   = toError $ "Unable to find a hosted zone zoned " ++ Text.unpack zone
