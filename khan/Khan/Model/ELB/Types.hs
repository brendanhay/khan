{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

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
    (
    -- * Frontend to Backend Mapping
      Mapping           (..)

    , Frontend
    , frontendProtocol
    , frontendPort

    , Backend
    , backendProtocol
    , backendPort
    , backendHealthCheck

    , HealthCheckTarget
    , healthCheckText

    , PortNumber
    , Protocol          (..)
    , protocolText

      -- * Naming
    , BalancerName
    , mkBalancerName
    , balancerNameText
    , balancerNamesFromASG
    , balancerNameFromDescription

    , PolicyType        (..)
    , PolicyName
    , mkPolicyName
    , policyNameText
    , policyNameFromCreateRequest
    ) where

import           Data.Attoparsec.Text
import           Data.Monoid
import           Data.String
import qualified Data.Text                    as Text
import           Data.Text.Buildable          (Buildable(..))
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import qualified Data.Text.Lazy.Builder.Int   as Build
import           Khan.Internal
import           Khan.Prelude                 hiding (takeWhile)
import           Network.AWS.AutoScaling      (AutoScalingGroup(..))
import           Network.AWS.ELB              hiding (Listener)
import           Network.Socket               (PortNumber(..))
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..), text)

data Protocol
    = HTTP
    | HTTPS
    | TCP
    | SSL
      deriving (Eq, Show)

instance Buildable Protocol where
    build = fromString . show

instance Pretty Protocol where
    pretty = text . show

instance TextParser Protocol where
    parser = f "https" HTTPS <|> f "http" HTTP <|> f "tcp" TCP <|> f "ssl" SSL
      where
        f x y = string x >> return y

data Mapping = Mapping
    { frontend :: !Frontend
    , backend  :: !Backend
    } deriving (Show)

instance TextParser Mapping where
    parser = Mapping <$> (parser <* char '-') <*> parser

newtype Frontend = FE Endpoint deriving (Show)

instance TextParser Frontend where
    parser = FE <$> parser

instance Pretty Frontend where
    pretty (FE ep) = "frontend/" <> pretty ep

frontendProtocol :: Frontend -> Protocol
frontendProtocol (FE ep) = endpointProtocol ep

frontendPort :: Frontend -> PortNumber
frontendPort (FE ep) = endpointPort ep

protocolText :: Protocol -> Text
protocolText = Text.toLower . Text.pack . show

data Backend = BE !Endpoint !HealthCheckTarget deriving (Show)

instance Pretty Backend where
    pretty (BE ep _) = "backend/" <> pretty ep

instance TextParser Backend where
    parser = BE <$> parser <*> (char ',' *> parser)

backendProtocol :: Backend -> Protocol
backendProtocol (BE ep _) = endpointProtocol ep

backendPort :: Backend -> PortNumber
backendPort (BE ep _) = endpointPort ep

backendHealthCheck :: Backend -> HealthCheckTarget
backendHealthCheck (BE _ hc) = hc

newtype HealthCheckTarget = HCT
    { healthCheckText :: Text
    } deriving (Show, Pretty)

instance TextParser HealthCheckTarget where
    parser = do
        x <- parser <* char ':'
        mk x <$> decimal <*> target x
      where
        mk :: Protocol -> PortNumber -> Maybe Text -> HealthCheckTarget
        mk x p f = HCT
             . LText.toStrict
             . Build.toLazyText
             $ build x
            <> ":"
            <> Build.decimal p
            <> maybe mempty build f

        target x = case x of
            HTTP  -> Just <$> path
            HTTPS -> Just <$> path
            TCP   -> pure Nothing
            SSL   -> pure Nothing

        path = mappend "/" <$> (char '/' *> takeWhile (inClass "-a-z-A-Z_0-9/"))

data Endpoint = EP
    { endpointProtocol :: !Protocol
    , endpointPort     :: !PortNumber
    } deriving (Show)

instance Pretty Endpoint where
    pretty (EP pr po) = pretty pr <> ":" <> pretty (toInteger po)

instance TextParser Endpoint where
    parser = EP <$> (parser <* char ':') <*> decimal

newtype BalancerName = BalancerName { balancerNameText :: Text }
    deriving (Eq, Show, Pretty)

balancerNamesFromASG :: AutoScalingGroup -> [BalancerName]
balancerNamesFromASG = map BalancerName . asgLoadBalancerNames

balancerNameFromDescription :: LoadBalancerDescription -> Maybe BalancerName
balancerNameFromDescription = fmap BalancerName . lbdLoadBalancerName

mkBalancerName :: Naming a => a -> Mapping -> BalancerName
mkBalancerName n = mk . frontendProtocol . frontend
  where
    mk p = BalancerName (balancerBaseName (names n) <> "-" <> protocolText p)

newtype PolicyName = PolicyName { policyNameText :: Text }
    deriving (Eq, Show, Pretty)

newtype PolicyType = PolicyType { policyTypeText :: Text }
    deriving (Eq, Show, Pretty)

mkPolicyName :: BalancerName -> PolicyType -> PolicyName
mkPolicyName b p = PolicyName (Text.intercalate "-" [balancerNameText b, t])
  where
    t = Text.toLower (fromMaybe x ("PolicyType" `Text.stripSuffix` x))
    x = policyTypeText p

policyNameFromCreateRequest :: CreateLoadBalancerPolicy -> PolicyName
policyNameFromCreateRequest = PolicyName . clbpPolicyName
