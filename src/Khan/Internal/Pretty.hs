{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Internal.Pretty
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Pretty
    ( prettyPrint
    ) where

import           Control.Monad.IO.Class
import           Data.List               (intersperse, transpose)
import           Data.Maybe
import qualified Data.Text               as Text
import           Data.Time               (UTCTime)
import qualified Data.Time               as Time
import           Network.AWS
import           Network.AWS.AutoScaling
import           Network.AWS.EC2
import           System.Locale
import           Text.PrettyPrint.Boxes

prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = liftIO . printBox . pretty

class Pretty a where
    pretty :: a -> Box

instance Pretty AutoScalingGroup where
    pretty AutoScalingGroup{..} = text name // layout [hs, vs]
      where
        name = "[" ++ Text.unpack asgAutoScalingGroupName ++ "] ->"

        hs = [ "zones"
             , "cooldown"
             , "min"
             , "max"
             , "desired"
             , "status"
             , "created"
             ]

        vs = [ zones
             , show asgDefaultCooldown
             , show asgMinSize
             , show asgMaxSize
             , show asgDesiredCapacity
             , maybe "OK" show asgStatus
             , formatISO8601 asgCreatedTime
             ]

        zones | null asgAvailabilityZones = ""
              | otherwise = (show . azRegion $ head asgAvailabilityZones)
                  ++ "["
                  ++ intersperse ',' (map azSuffix asgAvailabilityZones)
                  ++ "]"

instance Pretty [RunningInstancesItemType] where
    pretty = moveDown 1 . layout . (hs :) . map f
      where
        hs = [ "instance-id"
             , "image-id"
             , "public-ip"
             , "type"
             , "state"
             , "reason"
             , "weight"
             , "launched"
             ]

        f RunningInstancesItemType{..} =
            [ Text.unpack riitInstanceId
            , Text.unpack riitImageId
            , m "" riitIpAddress
            , show riitInstanceType
            , Text.unpack $ istName riitInstanceState
            , m "" riitStateReason
            , m "0" $ weight riitTagSet
            , formatISO8601 riitLaunchTime
            ]

        m d = Text.unpack . fromMaybe d

        weight = listToMaybe
            . map rtsitValue
            . filter ((== "Weight") . rtsitKey)

layout :: [[String]] -> Box
layout = moveRight 1
    . hsep 2 left
    . map (vcat left . map text)
    . transpose

formatISO8601 :: UTCTime -> String
formatISO8601 = Time.formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")
