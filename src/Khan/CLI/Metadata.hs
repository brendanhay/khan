{-# LANGUAGE NoImplicitPrelude   #-}
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

import qualified Data.Text.Encoding       as Text
import           Data.Text.Format
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2
import           Network.AWS.EC2.Metadata
import           Text.Show.Pretty

defineOptions "Info" $
    textOption "dId" "id" ""
        "Id of the instance to describe."

deriving instance Show Info

instance Discover Info where
    discover d
        | not $ invalid (dId d) = return d
        | otherwise = liftEitherT $ do
            iid <- Text.decodeUtf8 <$> metadata InstanceId
            return $! d { dId = iid }

instance Validate Info where
    validate Info{..} =
        check dId "--instance-id must be specified."

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
    [ subCommand "info" info
    ]

info :: Info -> AWS ()
info Info{..} = do
    log "Describing instance {}" [dId]
    is <- dirReservationSet <$> send (DescribeInstances [dId] [])
    log "{}" . Only $ ppShow is
