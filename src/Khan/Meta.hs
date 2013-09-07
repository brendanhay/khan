{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Khan.Meta
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Meta (meta) where

import           Control.Applicative
import           Control.Error
import qualified Data.ByteString.Char8    as BS
import           Data.Text.Encoding
import           Khan.Internal
import           Network.AWS
-- import           Network.AWS.EC2
import           Network.AWS.EC2.Metadata

defineOptions "Describe" $ do
    textOption "dInstanceId" "instance-id" ""
        "Id of the instance to describe."

    regionOption "dRegion" "region" Ireland
        "Region of the instance."

deriving instance Show Describe

instance Discover Describe where
    discover d = do
        iid <- decodeUtf8 <$> metadata InstanceId
        az  <- BS.unpack . BS.init <$> metadata AvailabilityZone
        reg <- fmapError $ tryRead ("Failed to read region from: " ++ az) az
        return $! d
            { dInstanceId = iid
            , dRegion     = reg
            }

instance Validate Describe where
    validate Describe{..} =
        check dInstanceId "--instance-id must be specified."

meta :: Command
meta = Command "meta" "Manage Instance Metadata."
    [ subCommand "describe" describe
    ]
  where
    describe d@Describe{..} = do
        logInfo $ "Describing instance " ++ show d ++ "..."
        -- r@DescribeTagsResponse{..} <- send $ DescribeTags
        --     [ TagResourceId [dInstanceId]
        --     ]

        -- logInfo $ show r

--        within dRegion


