{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.Tag.Tagged
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Tag.Tagged where

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import qualified Data.Text               as Text
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.AutoScaling as ASG
import qualified Network.AWS.EC2         as EC2

class Tagged a where
    tags :: a -> HashMap Text Text

instance Tagged a => Tagged [a] where
    tags = Map.unions . map tags

instance Tagged a => Tagged (Members a) where
    tags = tags . members

instance Tagged (HashMap Text Text) where
    tags = id

instance Tagged EC2.DescribeTagsResponse where
    tags = tags . EC2.dtagsrTagSet

instance Tagged EC2.TagSetItemType where
    tags EC2.TagSetItemType{..} = Map.singleton tsitKey tsitValue

instance Tagged EC2.RunningInstancesItemType where
    tags = tags . EC2.riitTagSet

instance Tagged EC2.ResourceTagSetItemType where
    tags EC2.ResourceTagSetItemType{..} = Map.singleton rtsitKey rtsitValue

instance Tagged ASG.AutoScalingGroup where
    tags = tags . ASG.asgTags

instance Tagged ASG.Tag where
    tags ASG.Tag{..} = Map.singleton tKey (fromMaybe Text.empty tValue)
