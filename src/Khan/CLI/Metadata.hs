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

module Khan.CLI.Metadata (commands) where

import           Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding         as Text
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata

defineOptions "Info" $
    textOption "dId" "id" ""
        "Id of the instance to describe. Supports discovery."

deriving instance Show Info

instance Discover Info where
    discover False d = return d
    discover True  d
        | not $ invalid (dId d) = return d
        | otherwise = liftEitherT $ do
            iid <- Text.decodeUtf8 <$> metadata InstanceId
            return $! d { dId = iid }

instance Validate Info where
    validate Info{..} =
        check dId "--instance-id must be specified."

commands :: [Command]
commands =
    [ command info "info" "Describe an instance." "Some help text"
    ]

info :: Info -> AWS ()
info Info{..} = do
    log "Describing instance {}" [dId]
    is <- EC2.findInstances [dId] []
    mapM_ (liftIO . LBS.putStrLn . Aeson.encodePretty . EC2.Instance) is
