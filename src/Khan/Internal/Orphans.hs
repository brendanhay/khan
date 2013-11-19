{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Khan.Internal.Orphans
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Orphans where

import           Data.Aeson
import qualified Data.Text           as Text
import           Data.Text.Buildable
import qualified Data.Text.Lazy      as LText
import           Khan.Prelude
import           Network.AWS.EC2
import           Network.AWS
import           Shelly

instance Buildable [Text] where
    build = build . Text.intercalate ", "

instance Buildable [LText.Text] where
    build = build . LText.intercalate ", "

instance Buildable FilePath where
    build = build . toTextIgnore

instance Buildable [InstanceId] where
    build = build . Text.intercalate ", " . map unInstanceId

instance Buildable ImageId where
    build = build . unImageId

instance ToJSON InstanceId where
    toJSON = toJSON . show . unInstanceId

instance ToJSON ImageId where
    toJSON = toJSON . show . unImageId

instance ToJSON InstanceType where
    toJSON = toJSON . show

instance ToJSON AvailabilityZone where
    toJSON = toJSON . show
