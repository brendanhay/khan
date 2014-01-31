{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Data.Aeson
import           Data.Aeson.Types             (Options(..), defaultOptions)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.SemVer
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy.Builder       as Build
import qualified Filesystem.Path.CurrentOS    as Path
import           GHC.Generics                 (Generic)
import           Khan.Internal.Text
import           Khan.Prelude
import           Network.AWS                  (Region)
import qualified Text.EDE.Filters             as EDE
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
    invalid Nothing  = True

instance Invalid Region where
    invalid _ = False

class ToEnv a where
    toEnv     :: a -> HashMap Text Text
    renderEnv :: Bool -> a -> Build.Builder

    renderEnv multi = Map.foldrWithKey (key suffix) mempty . toEnv
      where
        suffix | multi     = Build.singleton '\n'
               | otherwise = mempty

        key suf k v = mappend
             (Build.fromText (Text.toUpper $ EDE.underscore k)
           <> Build.singleton '='
           <> Build.fromText v
           <> suf)

instance ToEnv (Text, Text) where
    toEnv = Map.fromList . (:[])

instance ToEnv (HashMap Text Text) where
    toEnv = id

data Tags = Tags
    { tagRole    :: !Text
    , tagEnv     :: !Text
    , tagDomain  :: !Text
    , tagVersion :: Maybe Version
    , tagWeight  :: !Int
    } deriving (Eq, Ord, Show)

instance ToEnv Tags where
    toEnv Tags{..} = Map.fromList $
        [ ("ROLE",    tagRole)
        , ("ENV",     tagEnv)
        , ("DOMAIN",  tagDomain)
        , ("WEIGHT",  Text.pack $ show tagWeight)
        ] ++ maybeToList (("VERSION",) . showVersion <$> tagVersion)

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
    , policyName  :: !Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON Names where
    toJSON = genericToJSON $ defaultOptions
        { fieldLabelModifier =
            Text.unpack . (`mappend` "_name") . tstrip "Name" . Text.pack
        }

createNames :: Text -> Text -> Maybe Version -> Names
createNames role env ver = Names
    { envName     = env
    , keyName     = env <> "-khan"
    , roleName    = role
    , profileName = nameEnv
    , groupName   = nameEnv
    , imageName   = Text.concat [role, maybe "" (Text.cons '_') safeVer]
    , appName     = Text.concat [role, fromMaybe "" safeVer, ".", env]
    , versionName = showVersion <$> ver
    , policyName  = nameEnv
    }
  where
    nameEnv = Text.concat [env, "-", role]
    safeVer = safeVersion <$> ver

safeVersion :: Version -> Text
safeVersion = Text.map f . showVersion
  where
    f '+' = '/'
    f  c  = c

unversioned :: Text -> Text -> Names
unversioned role env = createNames role env Nothing

versioned :: Text -> Text -> Version -> Names
versioned role env = createNames role env . Just

class Naming a where
    names :: a -> Names

instance Naming Names where
    names = id

instance Naming Text where
    names t = Names t t t t t t t Nothing t

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
