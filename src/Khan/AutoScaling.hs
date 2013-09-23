{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.AutoScaling (command) where

import           Control.Applicative
import           Control.Concurrent     (threadDelay)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Khan.Internal
import           Network.AWS
import           Network.AWS.Route53
import           Pipes
import qualified Pipes.Prelude          as Pipes
import           Text.Show.Pretty

defineOptions "Group" $ do
    textOption "gName" "name" ""
        "Name of the group."

deriving instance Show Group

instance Discover Group
instance Validate Group

command :: Command
command = Command "autoscaling" "Manage Auto Scaling Groups."
    [ subCommand "describe" describe
    , subCommand "modify"   modify
    , subCommand "delete"   delete
    ]
  where
    describe Group{..} = return ()

    modify Group{..} = return ()

    delete Group{..} = return ()
