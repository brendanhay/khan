{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.EC2.Image
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.EC2.Image
    ( find
    , findAll
    , findAllCatch
    , create
    ) where

import           Control.Monad
import qualified Data.Text          as Text
import           Khan.Internal
import qualified Khan.Model.Tag     as Tag
import           Khan.Prelude       hiding (find, min, max)
import           Network.AWS.EC2    hiding (Instance, wait)

find :: [Text] -> [Filter] -> AWS DescribeImagesResponseItemType
find ids fs = findAll ids fs >>=
    hoistError . note "Failed to find any matching Images" . listToMaybe

findAll :: [Text] -> [Filter] -> AWS [DescribeImagesResponseItemType]
findAll ids fs = do
    if null ids
        then log_ "Searching for Images..."
        else say "Searching for Images {}" [L ids]
    djImagesSet <$> send (DescribeImages [] ids [] fs)

findAllCatch :: [Text]
             -> [Filter]
             -> AWS (Either EC2ErrorResponse [DescribeImagesResponseItemType])
findAllCatch ids fs =
    fmap djImagesSet <$> sendCatch (DescribeImages [] ids [] fs)

create :: Naming a => a -> Text -> [BlockDeviceMappingItemType] -> AWS Text
create (names -> n@Names{..}) i bs = do
    say "Creating Image {} from {}" [imageName, i]
    img <- cjImageId <$> send (CreateImage i imageName Nothing Nothing bs)
    wait limit img
    Tag.images n [img]
    return img
  where
    wait 0 img = throwAWS "Image creation failed: {}" [B img]
    wait l img = do
        say "Waiting {} seconds for Image {} creation..."
            [show delay, Text.unpack img]
        delaySeconds delay
        rs <- findAllCatch [img] []
        verifyEC2 "InvalidAMIID.NotFound" rs
        if pending (hush rs)
            then say "Image still pending: {}" [img] >> wait (l - 1) img
            else say "Image marked as available: {}" [img]

    pending (Just (x:_)) = diritImageState x /= "available"
    pending _            = True

    limit = 24 :: Int -- 24 * 20 seconds = 8 minutes
    delay = 20        -- 20 seconds
