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
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Khan.Internal
import           Network.AWS.EC2.Metadata

defineOptions "Describe" $ do
    textOption "dInstanceId" "instance-id" ""
        "Id of the instance to describe."

deriving instance Show Describe

instance Discover Describe where
    discover = return

instance Validate Describe where
    validate Describe{..} =
        check dInstanceId "--instance-id must be specified."

meta :: Command
meta = Command "meta" "Manage Instance Metadata."
    [ subCommand "describe" describe
    ]
  where
    describe d@Describe{..} = do
        logInfo $ show d
