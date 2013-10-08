{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

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
import           Control.Error
import qualified Data.ByteString.Char8    as BS
import           Data.Text.Encoding
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           Network.AWS.EC2.Metadata
import           Text.Show.Pretty
import qualified Khan.AWS.AutoScaling   as AutoScaling
import qualified Khan.AWS.EC2           as EC2
import qualified Khan.AWS.IAM           as IAM

defineOptions "Describe" $
    textOption "dInstanceId" "instance-id" ""
        "Id of the instance to describe."

deriving instance Show Describe

instance Discover Describe where
    discover d = liftEitherT $ do
        iid <- decodeUtf8 <$> metadata InstanceId
        return $! d { dInstanceId = iid }

instance Validate Describe where
    validate Describe{..} =
        check dInstanceId "--instance-id must be specified."

cli :: Command
cli = Command "metadata" "Manage Instance Metadata."
    [ subCommand "describe" describe
    ]
  where
    describe d@Describe{..} = do
        logInfo $ "Describing instance " ++ show d ++ "..."
        res <- send $ DescribeTags [TagResourceId [dInstanceId]]
        logInfo $ ppShow res
