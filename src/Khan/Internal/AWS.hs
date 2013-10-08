{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Internal.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.AWS where

import           Control.Applicative
import           Control.Arrow           ((***))
import           Control.Concurrent      (threadDelay)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable           (toList)
import           Data.List               ((\\), partition)
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Khan.Internal.Log
import           Khan.Internal.Types
import           Network.AWS
import           Network.AWS.AutoScaling hiding (Filter)
import           Network.AWS.EC2
import           Network.AWS.IAM
import           Network.AWS.Internal
import           Prelude                 hiding (min, max)
import           System.Directory

policyPath :: FilePath
policyPath = "./config/role-policy.json"

trustPath :: FilePath
trustPath = "./config/trust-relationship.json"

sshGroup :: Text -> Text
sshGroup = mappend "ssh-"

sshRules :: [IpPermissionType]
sshRules = [IpPermissionType TCP 22 22 [] [IpRange "0.0.0.0/0"]]

certPath :: FilePath
certPath = "./cert"

verifyAS :: Text -> Either AutoScalingErrorResponse a -> AWS ()
verifyAS  = (`verify` (aseCode . aserError))

verifyEC2 :: Text -> Either EC2ErrorResponse a -> AWS ()
verifyEC2 = (`verify` (ecCode . head . eerErrors))

verifyIAM :: Text -> Either IAMError a -> AWS ()
verifyIAM = (`verify` (etCode . erError))

verify :: (Eq a, ToError e) => a -> (e -> a) -> Either e b -> AWS ()
verify k f = checkError ((k ==) . f)
