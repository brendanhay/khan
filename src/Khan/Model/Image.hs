{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Model.Image
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Image
    ( find
    , findAll
    , create
    ) where

import Control.Arrow      ((***))
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.List          (partition)
import Khan.Internal
import Khan.Prelude       hiding (find, min, max)
import Network.AWS.EC2    hiding (Instance, wait)

find :: [Text] -> [Filter] -> AWS DescribeImagesResponseItemType
find ids fs = findAll ids fs >>=
    hoistError . note "Failed to find any matching Images" . listToMaybe

findAll :: [Text] -> [Filter] -> AWS [DescribeImagesResponseItemType]
findAll ids = fmap djImagesSet . send . DescribeImages [] ids []

create :: Text -> Text -> [BlockDeviceMappingItemType] -> AWS Text
create i n bs = do
    log "Creating Image {} from {}" [n, i]
    rid <- cjImageId <$> send (CreateImage i n Nothing Nothing bs)
    log "Waiting for Image {} creation..." [rid]
    wait [rid]
    log "Tagging Image {}" [rid]
    send_ $ CreateTags [rid] [ResourceTagSetItemType "Name" n]
    return rid

wait :: [Text] -> AWS ()
wait []  = log_ "All Images available."
wait ids = do
    xs <- findAll ids []
    let (ps, rs) = join (***) (map diritImageId) $ pending xs
    unless (null rs) $
        log "Images marked as available: {}" [rs]
    unless (null ps) $ do
        log "Images still pending: {}" [ps]
        log_ "Waiting..."
        liftIO . threadDelay $ 1000000 * 30
    wait ps
  where
    pending = partition (("available" /=) . diritImageState)
