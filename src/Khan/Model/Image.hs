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
    , findAllCatch
    , create
    ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import qualified Data.Text          as Text
import           Khan.Internal
import           Khan.Prelude       hiding (find, min, max)
import           Network.AWS.EC2    hiding (Instance, wait)

find :: [Text] -> [Filter] -> AWS DescribeImagesResponseItemType
find ids fs = findAll ids fs >>=
    hoistError . note "Failed to find any matching Images" . listToMaybe

findAll :: [Text] -> [Filter] -> AWS [DescribeImagesResponseItemType]
findAll ids fs = djImagesSet <$>
    send (DescribeImages [] ids [] fs)

findAllCatch :: [Text]
             -> [Filter]
             -> AWS (Either EC2ErrorResponse [DescribeImagesResponseItemType])
findAllCatch ids fs = either Left (Right . djImagesSet) <$>
    sendCatch (DescribeImages [] ids [] fs)

create :: Text -> Text -> [BlockDeviceMappingItemType] -> AWS Text
create i n bs = do
    log "Creating Image {} from {}" [n, i]
    img <- cjImageId <$> send (CreateImage i n Nothing Nothing bs)
    wait limit img
    log "Tagging Image {}" [img]
    send_ $ CreateTags [img] [ResourceTagSetItemType "Name" n]
    return img
  where
    wait 0 img = throwAWS "Image creation failed: {}" [img]
    wait l img = do
        log "Waiting {} seconds for Image {} creation..."
            [show delay, Text.unpack img]
        liftIO $ threadDelay delay
        rs <- findAllCatch [img] []
        verifyEC2 "InvalidAMIID.NotFound" rs
        if pending (hush rs)
            then log "Image still pending: {}" [img] >> wait (l - 1) img
            else log "Image marked as available: {}" [img]

    pending (Just (x:_)) = diritImageState x /= "available"
    pending _            = True

    limit = 24 :: Int    -- 24 * 20 seconds = 8 minutes
    delay = 1000000 * 20 -- 20 seconds
