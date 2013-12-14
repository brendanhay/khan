{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Text                    as Text
import           Data.Version
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Prelude
import           Network.AWS                  (Region)
import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.Read
import qualified Text.Read                    as Read

class Invalid a where
    invalid :: a -> Bool
    valid   :: a -> Bool

    valid = not . invalid

instance Invalid Bool where
    invalid = id

instance Invalid Text where
    invalid = Text.null

instance Invalid a => Invalid [a] where
    invalid [] = True
    invalid xs = invalid `any` xs

instance Invalid (NonEmpty a) where
    invalid _ = False

instance Invalid Char where
    invalid _ = False

instance Invalid FilePath where
    invalid f
        | Path.null f = True
        | otherwise   = not $ Path.valid f

instance Invalid a => Invalid (Maybe a) where
    invalid (Just x) = invalid x
    invalid Nothing  = False

instance Invalid Region where
    invalid _ = False

data Tags = Tags
    { tagRole    :: !Text
    , tagEnv     :: !Text
    , tagDomain  :: !Text
    , tagVersion :: Maybe Version
    } deriving (Eq, Ord, Show)

data DNS = DNS
    { dnsRole :: !Text
    , dnsVer  :: Maybe Version
    , dnsOrd  :: !Integer
    , dnsEnv  :: !Text
    , dnsReg  :: !Text
    } deriving (Eq, Ord, Show)

data Names = Names
    { envName     :: !Text
    , keyName     :: !Text
    , roleName    :: !Text
    , profileName :: !Text
    , groupName   :: !Text
    , imageName   :: !Text
    , appName     :: !Text
    , versionName :: Maybe Text
    } deriving (Eq, Ord, Show)

createNames :: Text -> Text -> Maybe Version -> Names
createNames role env ver = Names
    { envName     = env
    , keyName     = env <> "-khan"
    , roleName    = role
    , profileName = nameEnv
    , groupName   = nameEnv
    , imageName   = Text.concat [role, maybe "" (Text.cons '_') safeVer]
    , appName     = Text.concat [role, fromMaybe "" safeVer, ".", env]
    , versionName = safeVer
    }
  where
    nameEnv = Text.concat [env, "-", role]
    safeVer = safeVersion <$> ver

-- FIXME: correctly add build number suffix

safeVersion :: Version -> Text
safeVersion ver = Text.pack $
    case ver of
        (Version (ma:mi:p:_) b) ->
            concat [show ma, "m", show mi, "p", show p, "b", filter f $ concat b]
        _ -> showVersion ver
  where
    f '+' = False
    f  _  = True

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

readAssocList :: [(String, a)] -> Read.ReadPrec a
readAssocList xs = Read.choice $
    map (\(x, y) -> Read.lift $ ReadP.string x >> return y) xs
