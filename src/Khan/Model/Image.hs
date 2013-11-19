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
    ) where

import qualified Data.Text       as Text
import           Khan.Prelude    hiding (find, min, max)
import           Network.AWS.EC2 hiding (Instance)

find :: [Filter] -> AWS ImageId
find fs = do
    log "Finding Images matching: {}" [options]
    rs  <- fmap (listToMaybe . djImagesSet) . send $ DescribeImages [] [] [] fs
    ami <- fmap diritImageId . hoistError $
        note "Failed to find any matching Images" rs
    log "Found Image {} matching {}" [unImageId ami, options]
    return ami
  where
    options = Text.intercalate " | " $ map (Text.pack . show) fs
