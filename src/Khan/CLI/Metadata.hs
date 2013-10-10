{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.CLI.Metadata
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Metadata (cli) where

import           Control.Applicative
import qualified Data.Text.Encoding       as Text
import           Data.Text.Format
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           Network.AWS.EC2.Metadata
import           Text.Show.Pretty

defineOptions "Tags" $
    textOption "dInstance" "id" ""
        "Id of the instance to describe."

deriving instance Show Tags

instance Discover Tags where
    discover d
        | not $ invalid (dInstance d) = return d
        | otherwise = liftEitherT $ do
            iid <- Text.decodeUtf8 <$> metadata InstanceId
            return $! d { dInstance = iid }

instance Validate Tags where
    validate Tags{..} =
        check dInstance "--instance-id must be specified."

defineOptions "Local" $
    textOption "dPath" "path" ""
        "Metadata path to retrieve."

deriving instance Show Local

instance Discover Local

instance Validate Local where
    validate Local{..} =
        check dPath "--path must be specified."

cli :: Command
cli = Command "metadata" "Manage Instance Metadata."
    [ subCommand "tags"  tags
    ]

tags :: Tags -> AWS ()
tags Tags{..} = do
    logInfo "Describing instance {}" [dInstance]
    send (DescribeTags [TagResourceId [dInstance]]) >>=
        logInfo "{}" . Only . ppShow
