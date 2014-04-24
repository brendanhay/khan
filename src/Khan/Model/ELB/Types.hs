{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

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
    ( -- * Backend to Frontend Mapping
      Mapping  (..)
    , Frontend
    , Backend
    , Protocol (..)
    , Port
    , protocol
    , protocolText
    , port
    , healthCheck

      -- * Naming
    , Name (nameText)
    , mkName
    , namesFromASG
    , nameFromDescription
    ) where

import Data.Attoparsec.Text
import Data.String                  (fromString)
import Data.Text                    (Text, toLower, pack, unpack)
import Data.Text.Buildable          (Buildable (..))
import Data.Text.Lazy               (toStrict)
import Data.Text.Lazy.Builder       (toLazyText)
import Khan.Internal
import Network.AWS.AutoScaling      (AutoScalingGroup (..))
import Network.AWS.ELB              hiding (Listener)
import Prelude                      hiding (takeWhile)
import Text.PrettyPrint.ANSI.Leijen (Pretty (..), text)

import qualified Data.Text.Lazy.Builder.Int as Build

-------------------------------------------------------------------------------
-- Frontend to Backend Mapping

type Port = Integer

data Protocol
    = HTTP
    | HTTPS
    | TCP
    | SSL
    deriving (Show)

instance Buildable Protocol where
    build = fromString . show

data Mapping = Mapping
    { frontend :: !Frontend
    , backend  :: !Backend
    } deriving (Show)

class Listener a where
    protocol :: a -> Protocol
    port     :: a -> Port

data Frontend = FE !Protocol !Port
    deriving (Show)

instance Listener Frontend where
    protocol (FE s _) = s
    port     (FE _ p) = p

data Backend = BE !Protocol !Port !Text
    deriving (Show)

instance Listener Backend where
    protocol (BE s _ _) = s
    port     (BE _ p _) = p

healthCheck :: Backend -> Text
healthCheck (BE s p c) = toStrict . toLazyText $
    build (proto s) <> ":" <> Build.decimal p <> build c
  where
    proto HTTP  = HTTP
    proto HTTPS = HTTPS
    proto TCP   = HTTP
    proto SSL   = HTTPS

protocolText :: Protocol -> Text
protocolText = toLower . pack . show

instance TextParser Protocol where
    parser = do
        p <- takeTill (== ':') <* char ':'
        case p of
            "http"  -> return HTTP
            "https" -> return HTTPS
            "tcp"   -> return TCP
            "ssl"   -> return SSL
            s       -> fail (unpack s)

instance TextParser Frontend where
    parser = FE <$> parser <*> decimal

instance TextParser Backend where
    parser = do
        pr <- parser
        po <- decimal
        ep <- char '/' *> takeWhile (inClass "-a-zA-Z_0-9/")
        return $ BE pr po ("/" <> ep)

instance TextParser Mapping where
    parser = do
        fe <- parser
        skipSpace
        _  <- asciiCI "TO"
        skipSpace
        be <- parser
        return $ Mapping fe be

instance Pretty Protocol where
    pretty = text . show

instance Pretty Frontend where
    pretty (FE s p) = "frontend/" <> pretty s <> ":" <> pretty p

instance Pretty Backend where
    pretty (BE s p c) = "backend/" <> pretty s <> ":" <> pretty p <> pretty c

-------------------------------------------------------------------------------
-- Naming

newtype Name = Name { nameText :: Text } deriving (Eq, Show, Pretty)

namesFromASG :: AutoScalingGroup -> [Name]
namesFromASG = map Name . asgLoadBalancerNames

nameFromDescription :: LoadBalancerDescription -> Maybe Name
nameFromDescription = fmap Name . lbdLoadBalancerName

mkName :: Naming a => a -> Mapping -> Name
mkName n = mk . protocol . frontend
  where
    mk p = Name $ balancerBaseName (names n) <> "-" <> protocolText p
