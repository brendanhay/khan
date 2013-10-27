{-# LANGUAGE NoImplicitPrelude #-}
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

import           Data.Attoparsec.Text
import           Data.List                    ((\\))
import qualified Data.Text                    as Text
import           Data.Version
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Prelude
import           Network.AWS.EC2
import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.Read
import qualified Text.Read                    as Read

class Discover a where
    discover :: Bool -> a -> AWS a
    discover _ = return

class Validate a where
    validate :: MonadIO m => a -> EitherT AWSError m ()
    validate = void . return

class Invalid a where
    invalid :: a -> Bool

instance Invalid Bool where
    invalid = id

instance Invalid Text where
    invalid = Text.null

instance Invalid Integer where
    invalid = (< 1)

instance Invalid a => Invalid [a] where
    invalid [] = True
    invalid xs = invalid `any` xs

instance Invalid Char where
    invalid _ = False

instance Invalid FilePath where
    invalid "" = True
    invalid f  = not $ Path.valid f

instance Invalid a => Invalid (Maybe a) where
    invalid (Just x) = invalid x
    invalid Nothing  = False

data Within a = Within [a] [a]

instance Eq a => Invalid (Within a) where
    invalid (Within xs ys) = not . null $ xs \\ ys

data Names = Names
    { envName     :: !Text
    , keyName     :: !Text
    , roleName    :: !Text
    , profileName :: !Text
    , groupName   :: !Text
    , imageName   :: !Text
    , appName     :: !Text
    , versionName :: Maybe Text
    }

createNames :: Text -> Text -> Maybe Version -> Names
createNames role env ver = Names
    { envName     = env
    , keyName     = env <> "-khan"
    , roleName    = role
    , profileName = nameEnv
    , groupName   = nameEnv
    , imageName   = Text.concat [role, "_", tver]
    , appName     = Text.concat [role, tver, ".", env]
    , versionName = mver
    }
  where
    nameEnv = Text.concat [env, "-", role]

    tver = fromMaybe "" mver
    mver = safeVersion <$> ver

unversioned :: Text -> Text -> Names
unversioned role env = createNames role env Nothing

versioned :: Text -> Text -> Version -> Names
versioned role env = createNames role env . Just

class Naming a where
    names :: a -> Names

instance Naming Names where
    names = id

instance Naming Text where
    names t = Names t t t t t t t Nothing

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

parseVersionE :: String -> Either String Version
parseVersionE s = maybe (Left $ "Failed to parse version: " ++ s) (Right . fst)
    . listToMaybe
    . reverse
    . ReadP.readP_to_S parseVersion
    $ map f s
  where
    f '+' = '-'
    f  c  = c

safeVersion :: Version -> Text
safeVersion = Text.map f . Text.pack . showVersion
  where
    f '-' = '/'
    f  c  = c

showRules :: Foldable f => f IpPermissionType -> Text
showRules = Text.intercalate ", " . map rule . toList
  where
    rule IpPermissionType{..} = Text.intercalate ":"
        [ Text.pack $ show iptIpProtocol
        , Text.pack $ show iptFromPort
        , Text.pack $ show iptToPort
        , fromMaybe "" groupOrRange
        ]
      where
        groupOrRange = headMay (mapMaybe uigGroupName $ toList iptGroups)
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

readAssocList :: [(String, a)] -> Read.ReadPrec a
readAssocList xs = Read.choice $
    map (\(x, y) -> Read.lift $ ReadP.string x >> return y) xs
