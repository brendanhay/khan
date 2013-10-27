{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.CLI.Routing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Routing (commands) where

import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Khan.AWS.EC2             as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2          hiding (ec2)
import           Network.AWS.EC2.Metadata

defineOptions "Routes" $ do
    textOption "rDomain" "domain" ""
        "DNS domain restriction. (required)"

    textOption "rEnv" "env" defaultEnv
        "Environment to describe."

    textsOption "rRoles" "roles" []
        "Roles to restrict to. (default: all)"

    stringOption "rZones" "zones" ""
         "Availability zones suffixes restriction. (discovered)"

    formatOption "rFmt" "format" Visual
        "Output format, supports visual or haproxy."

deriving instance Show Routes

instance Discover Routes where
    discover ec2 r' = do
        r@Routes{..} <- if ec2 then populate else return r'
        zs <- map (azSuffix . azitZoneName) <$> EC2.findCurrentZones
        log "Using Availability Zones '{}'" [zs]
        return $! r { rZones = zs }
      where
        populate = do
            iid      <- liftEitherT $ Text.decodeUtf8 <$> metadata InstanceId
            Tags{..} <- requiredTags iid
            return $! r' { rDomain = tagDomain, rEnv = tagEnv }

instance Validate Routes where
    validate Routes{..} = do
        check rEnv    "--env must be specified."
        check rDomain "--domain must be specified."
        check (Within rZones "abcde") "--zones must be within [a-e]."

commands :: [Command]
commands =
    [ command routes "routes" "Describe an environment's routing table."
        "Some help text"
    ]

routes :: Routes -> AWS ()
routes Routes{..} = do
    log "Describing environment {}" [rEnv]
    reg <- getRegion
    is  <- fmap dirReservationSet . send $ DescribeInstances []
        [ Filter "availability-zone" $ map (Text.pack . show . AZ reg) rZones
        , Filter ("tag:" <> envTag) [rEnv]
        , Filter ("tag:" <> roleTag) rRoles
        ]
    mapM_ (liftIO . print) is
