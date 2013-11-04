{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Khan.AWS.EC2               as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2            hiding (ec2)
import           Network.AWS.EC2.Metadata

data Routes = Routes
    { rEnv    :: !Text
    , rDomain :: !Text
    , rRoles  :: [Text]
    , rZones  :: !String
    , rFmt    :: OutputFormat
    }

routesParser :: Parser Routes
routesParser = Routes
    <$> textOption "env" "Environment to describe." (value defaultEnv)
    <*> textOption "domain" "DNS domain restriction." (value "")
    <*> many (textOption "role" "Role to restrict to." mempty)
    <*> stringOption "zones" "Availability zones suffixes restriction." (value "")
    <*> readOption "format" "FORMAT" "Output format, supports json or haproxy." (value JSON)

instance Options Routes where
    discover r@Routes{..} = do
        ec2 <- isEC2
        zs  <- EC2.defaultZoneSuffixes rZones
        log "Using Availability Zones '{}'" [zs]
        if not ec2
            then return $! r { rZones = zs }
            else do
                iid <- liftEitherT $ Text.decodeUtf8 <$> metadata InstanceId
                Tags{..} <- findRequiredTags iid
                return $! r { rDomain = tagDomain, rEnv = tagEnv, rZones = zs }

    validate Routes{..} = do
        check rEnv    "--env must be specified."
        check rDomain "--domain must be specified."
        check (Within rZones "abcde") "--zones must be within [a-e]."

commands :: Mod CommandFields Command
commands = command "routes" routes routesParser
    "Describe an environment's routing table."
  where
    routes Common{..} Routes{..} = do
        log "Describing environment {}" [rEnv]
        is <- EC2.findInstances [] fs
        mapM_ (liftIO . LBS.putStrLn . Aeson.encodePretty . EC2.Instance) is
      where
        zone = Text.pack . show . AZ cRegion

        fs = [ Filter "availability-zone" $ map zone rZones
             , Filter ("tag:" <> envTag)    [rEnv]
             , Filter ("tag:" <> domainTag) [rDomain]
             ] ++ if null rRoles
                      then []
                      else [Filter ("tag:" <> roleTag) rRoles]
