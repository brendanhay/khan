{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Khan.Internal.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Types where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Foldable                (Foldable, toList)
import           Data.List                    (intercalate)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Version
import           Network.AWS.EC2
import           Network.AWS.Internal
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.Read

class Discover a where
    discover :: a -> AWS a
    discover = return

class Validate a where
    validate :: Monad m => a -> EitherT Error m ()
    validate = void . return

class Invalid a where
    invalid :: a -> Bool

instance Invalid Bool where
    invalid = id

instance Invalid Text where
    invalid = Text.null

instance Invalid Integer where
    invalid = (< 1)

instance Invalid [a] where
    invalid = null

data RoutingPolicy
    = Failover
    | Latency
    | Weighted
    | Basic
      deriving (Eq)

instance Read RoutingPolicy where
    readPrec = readAssocList
        [ ("failover", Failover)
        , ("latency",  Latency)
        , ("weighted", Weighted)
        , ("basic",    Basic)
        ]

instance Show RoutingPolicy where
    show Failover = "failover"
    show Latency  = "latency"
    show Weighted = "weighted"
    show Basic    = "basic"

showRules :: Foldable f => f IpPermissionType -> String
showRules = intercalate ", " . map rule . toList
  where
    rule IpPermissionType{..} = intercalate ":"
        [ show iptIpProtocol
        , show iptFromPort
        , show iptToPort
        , Text.unpack $ fromMaybe "" groupOrRange
        ]
      where
        groupOrRange =
                 headMay (mapMaybe uigGroupName $ toList iptGroups)
             <|> headMay (map irCidrIp $ toList iptIpRanges)

parseRule :: String -> Either String IpPermissionType
parseRule = fmapL (++ " - expected tcp|udp|icmp:from_port:to_port:group|0.0.0.0")
    . parseOnly parser
    . Text.pack
  where
    parser = do
        p <- protocol
        f <- decimal <* char ':'
        t <- decimal <* char ':'
        g <- eitherP range group

        let perm = IpPermissionType p f t

        return $! either (\x -> perm [] [x])
                         (\x -> perm [x] []) g

    range = do
        a <- takeTill (== '.') <* char '.'
        b <- takeTill (== '.') <* char '.'
        c <- takeTill (== '.') <* char '.'
        d <- Text.pack <$> many1 anyChar
        return . IpRange $ Text.intercalate "." [a, b, c, d]

    group = UserIdGroupPair Nothing Nothing <$> (Just <$> takeText)

    protocol = do
        p <- takeTill (== ':') <* char ':'
        case p of
            "tcp"  -> return TCP
            "udp"  -> return UDP
            "icmp" -> return ICMP
            _      -> fail "Failed to parsed protocol"

defaultVersion :: Version
defaultVersion = Version [0] []

parseVersionE :: String -> Either String Version
parseVersionE s = maybe (Left $ "Failed to parse version: " ++ s) (Right . fst)
    . listToMaybe
    . reverse
    . readP_to_S parseVersion
    $ map f s
  where
    f '+' = '-'
    f  c  = c

safeVersion :: Version -> Text
safeVersion = Text.map f . Text.pack . showVersion
  where
    f '-' = '/'
    f  c  = c
