{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Artifact
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Artifact (command) where

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

defineOptions "Object" $ do
    textOption "oName" "name" ""
        "Name of the object."

deriving instance Show Object

instance Discover Object
instance Validate Object

defineOptions "Bucket" $ do
    textOption "bName" "name" ""
        "Name of the bucket."

deriving instance Show Bucket

instance Discover Bucket
instance Validate Bucket

command :: Command
command = Command "artifact" "Manage S3 Artifacts."
    [ subCommand "put"  put
    , subCommand "get"  get
    , subCommand "sync" sync
    ]
  where
    put Object{..} = return ()

    get Object{..} = return ()

    sync Bucket{..} = return ()
