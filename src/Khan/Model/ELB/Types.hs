{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

-- Module      : Khan.Model.ELB.HealthCheck
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.ELB.Types
    ( -- * Frontend to Backend Mapping
      Mapping           (..)
    , Frontend
    , Backend
    , HealthCheckTarget (healthCheckText)
    , Protocol          (..)
    , PortNumber
    , backendProtocol
    , backendPort
    , backendHealthCheck
    , frontendProtocol
    , frontendPort
    , protocolText

      -- * Naming
    , BalancerName (balancerNameText)
    , mkBalancerName
    , balancerNamesFromASG
    , balancerNameFromDescription

    , PolicyName   (policyNameText)
    , mkPolicyName
    , policyNameFromCreateRequest

    , PolicyType   (.. )
    ) where

import Data.Attoparsec.Text
import Data.String                  (fromString)
import Data.Text                    (Text, intercalate, pack, toLower, unpack, stripSuffix)
import Data.Text.Buildable          (Buildable (..))
import Data.Text.Lazy               (toStrict)
import Data.Text.Lazy.Builder       (toLazyText)
import Khan.Internal
import Network.AWS.AutoScaling      (AutoScalingGroup (..))
import Network.AWS.ELB              hiding (Listener)
import Network.Socket               (PortNumber (..))
import Prelude                      hiding (takeWhile)
import Text.PrettyPrint.ANSI.Leijen (Pretty (..), text)

import qualified Data.Text.Lazy.Builder.Int as Build

-------------------------------------------------------------------------------
-- Frontend to Backend Mapping

data Protocol
    = HTTP
    | HTTPS
    | TCP
    | SSL
    deriving (Eq, Show)

instance Buildable Protocol where
    build = fromString . show

data Mapping = Mapping
    { frontend :: !Frontend
    , backend  :: !Backend
    } deriving (Show)

newtype Frontend = FE Endpoint deriving (Show)

newtype HealthCheckTarget = HCT
    { healthCheckText :: Text
    } deriving (Show, Pretty)

data Backend = BE !Endpoint !HealthCheckTarget deriving (Show)

data Endpoint = EP
    { endpointProtocol :: !Protocol
    , endpointPort     :: !PortNumber
    } deriving (Show)

backendProtocol :: Backend -> Protocol
backendProtocol (BE ep _) = endpointProtocol ep

backendPort :: Backend -> PortNumber
backendPort (BE ep _) = endpointPort ep

backendHealthCheck :: Backend -> HealthCheckTarget
backendHealthCheck (BE _ hc) = hc

frontendProtocol :: Frontend -> Protocol
frontendProtocol (FE ep) = endpointProtocol ep

frontendPort :: Frontend -> PortNumber
frontendPort (FE ep) = endpointPort ep

mkHealthCheck :: Endpoint -> Text -> HealthCheckTarget
mkHealthCheck (EP pr po) path =
    HCT . toStrict . toLazyText $ build (proto pr)
        <> ":"
        <> Build.decimal po
        <> build path
  where
    proto HTTP  = HTTP
    proto HTTPS = HTTPS
    proto TCP   = HTTP
    proto SSL   = HTTPS

protocolText :: Protocol -> Text
protocolText = toLower . pack . show

instance TextParser Mapping where
    parser = do
        fe <- parser
        _  <- char '-'
        be <- parser
        return $ Mapping fe be

instance TextParser Frontend where
    parser = FE <$> parser

instance TextParser Backend where
    parser = do
        ep <- parser
        hc <- char '/' *> takeWhile (inClass "-a-zA-Z_0-9/")
        return $ BE ep (mkHealthCheck ep ("/" <> hc))

instance TextParser Endpoint where
    parser = do
        prot <- takeTill (== ':')
        _    <- char ':'
        port <- decimal
        case prot of
            "http"  -> return $ EP HTTP  port
            "https" -> return $ EP HTTPS port
            "tcp"   -> return $ EP TCP   port
            "ssl"   -> return $ EP SSL   port
            s       -> fail (unpack s)

instance Pretty Protocol where
    pretty = text . show

instance Pretty Endpoint where
    pretty (EP pr po) = pretty pr <> ":" <> pretty (toInteger po)

instance Pretty Frontend where
    pretty (FE ep) = "frontend/" <> pretty ep

instance Pretty Backend where
    pretty (BE ep _) = "backend/" <> pretty ep

-------------------------------------------------------------------------------
-- Naming

newtype BalancerName = BalancerName { balancerNameText :: Text }
    deriving (Eq, Show, Pretty)

balancerNamesFromASG :: AutoScalingGroup -> [BalancerName]
balancerNamesFromASG = map BalancerName . asgLoadBalancerNames

balancerNameFromDescription :: LoadBalancerDescription -> Maybe BalancerName
balancerNameFromDescription = fmap BalancerName . lbdLoadBalancerName

mkBalancerName :: Naming a => a -> Mapping -> BalancerName
mkBalancerName n = mk . frontendProtocol . frontend
  where
    mk p = BalancerName $ balancerBaseName (names n) <> "-" <> protocolText p


newtype PolicyName = PolicyName { policyNameText :: Text }
    deriving (Eq, Show, Pretty)

newtype PolicyType = PolicyType { policyTypeText :: Text }
    deriving (Eq, Show, Pretty)

mkPolicyName :: BalancerName -> PolicyType -> PolicyName
mkPolicyName b p = PolicyName . intercalate "-" $ [balancerNameText b, typ]
  where
    typ = toLower $ case policyTypeText p of
        (stripSuffix "PolicyType" -> Just pre) -> pre
        x                                      -> x

policyNameFromCreateRequest :: CreateLoadBalancerPolicy -> PolicyName
policyNameFromCreateRequest = PolicyName . clbpPolicyName
