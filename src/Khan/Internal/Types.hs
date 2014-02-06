{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

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
import           Data.String
import qualified Data.Text                    as Text
import           Data.Text.Buildable
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

type EnvMap = HashMap Text Text

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
    { tagRole    :: !Role
    , tagEnv     :: !Env
    , tagDomain  :: !Text
    , tagName    :: Maybe Text
    , tagVersion :: Maybe Version
    , tagWeight  :: !Int
    } deriving (Eq, Ord, Show)

instance ToEnv Tags where
    toEnv Tags{..} = Map.fromList $
        [ ("ROLE",    _role tagRole)
        , ("ENV",     _env tagEnv)
        , ("WEIGHT",  Text.pack $ show tagWeight)
        ] ++ maybeToList (("VERSION",) . showVersion <$> tagVersion)
          ++ maybeToList (("NAME",) <$> tagName)

instance Naming Tags where
    names Tags{..} = createNames tagRole tagEnv tagVersion

data DNS = DNS
    { dnsRole :: !Role
    , dnsVer  :: Maybe Version
    , dnsOrd  :: !Integer
    , dnsEnv  :: !Env
    , dnsReg  :: !Text
    } deriving (Eq, Ord, Show)

data Names = Names
    { envName      :: !Text
    , keyName      :: !Text
    , roleName     :: !Text
    , profileName  :: !Text
    , groupName    :: !Text
    , sshGroupName :: !Text
    , imageName    :: !Text
    , appName      :: !Text
    , versionName  :: Maybe Text
    , policyName   :: !Text
    } deriving (Eq, Ord, Show, Generic)

instance ToJSON Names where
    toJSON = genericToJSON $ defaultOptions
        { fieldLabelModifier =
            Text.unpack . (`mappend` "_name") . tstrip "Name" . Text.pack
        }

createNames :: Role -> Env -> Maybe Version -> Names
createNames (_role -> role) (_env -> env) ver = Names
    { envName      = env
    , keyName      = env <> "-khan"
    , roleName     = role
    , profileName  = nameEnv
    , groupName    = nameEnv
    , sshGroupName = env <> "-ssh"
    , imageName    = roleVer
    , appName      = env <> "-" <> roleVer
    , versionName  = showVersion <$> ver
    , policyName   = nameEnv
    }
  where
    nameEnv = env <> "-" <> role
    roleVer = role <> maybe "" (Text.cons '_') (safeVersion <$> ver)

safeVersion :: Version -> Text
safeVersion = Text.map f . showVersion
  where
    f '+' = '/'
    f  c  = c

unversioned :: Role -> Env -> Names
unversioned role env = createNames role env Nothing

versioned :: Role -> Env -> Version -> Names
versioned role env = createNames role env . Just

class Naming a where
    names :: a -> Names

instance Naming Names where
    names = id

instance Naming Text where
    names t = createNames (Role t) (Env t) Nothing

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

newtype Env = Env { _env :: Text }
    deriving (Eq, Ord, Show, IsString, Invalid, Buildable, Naming)

newtype Role = Role { _role :: Text }
    deriving (Eq, Ord, Show, IsString, Invalid, Buildable, Naming)

newtype TrustPath = TrustPath { _trust :: FilePath }
    deriving (Eq, Show, IsString)

newtype PolicyPath = PolicyPath { _policy :: FilePath }
    deriving (Eq, Show, IsString)
