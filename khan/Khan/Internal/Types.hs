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

module Khan.Internal.Types
   (
   -- * Aliases
     EnvMap

   -- * Classes
   , Invalid       (..)
   , ToEnv         (..)
   , Naming        (..)

   -- * Types
   , Modified      (..)
   , Ann           (..)
   , Tags          (..)
   , Names         (..)
   , RoutingPolicy (..)
   , TrustPath     (..)
   , PolicyPath    (..)
   , Env           (_env)
   , Role          (_role)

   -- * Smart constructors
   , versioned
   , unversioned
   , newEnv
   , newRole

   -- * Ghetto lenses
   , modified

   -- * Text helpers
   , regionToText
   , stripText
   ) where

import           Data.Aeson
import           Data.Aeson.Types             (Options(..), defaultOptions)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.Hashable
import           Data.SemVer                  (Version)
import qualified Data.SemVer                  as Ver
import qualified Data.SemVer.Delimited        as Delim
import           Data.String
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import qualified Filesystem.Path.CurrentOS    as Path
import           GHC.Generics                 (Generic)
import           Khan.Prelude
import           Lens.Family
import           Network.AWS                  (Region)
import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.Read
import qualified Text.Read                    as Read

type EnvMap = HashMap Text Text

data Modified a
    = Changed   { result :: !a }
    | Unchanged { result :: !a }
      deriving (Eq, Show)

modified :: Modified a -> Bool
modified (Changed _) = True
modified _           = False

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
    renderEnv :: Bool -> a -> LText.Text

    renderEnv multi = Build.toLazyText
        . Map.foldrWithKey (key suffix) mempty
        . toEnv
      where
        suffix
            | multi     = Build.singleton '\n'
            | otherwise = Build.singleton ' '

        key suf k v = mappend
            ( Build.fromText k
           <> Build.singleton '='
           <> Build.fromText v
           <> suf
            )

instance ToEnv (Text, Text) where
    toEnv = Map.fromList . (:[])

instance ToEnv (HashMap Text Text) where
    toEnv = id

data Ann a = Ann
    { annValue :: a
    , annTags  :: Tags
    }

data Tags = Tags
    { tagRole    :: !Role
    , tagEnv     :: !Env
    , tagDomain  :: !Text
    , tagName    :: Maybe Text
    , tagVersion :: Maybe Version
    , tagWeight  :: !Int
    , tagGroup   :: Maybe Text
    } deriving (Eq, Ord, Show)

-- Compatibility note:
-- Weight has been removed from the tag->sourceable environment output due to
-- to caching (see Tag.cached).
--
-- This is because of requiring the cached output's input to be immutable,
-- which KHAN_WEIGHT does not guarantee.
instance ToEnv Tags where
    toEnv Tags{..} = Map.fromList $
        [ ("KHAN_ROLE", _role tagRole)
        , ("KHAN_ENV", _env tagEnv)
        , ("KHAN_DOMAIN", tagDomain)
        ] ++ maybeToList (("KHAN_VERSION",) . Ver.toText <$> tagVersion)
          ++ maybeToList (("KHAN_NAME",) <$> tagName)

instance Naming Tags where
    names Tags{..} = createNames tagRole tagEnv tagVersion

data Names = Names
    { envName          :: !Text
    , keyName          :: !Text
    , roleName         :: !Text
    , profileName      :: !Text
    , groupName        :: !Text
    , imageName        :: !Text
    , appName          :: !Text
    , balancerBaseName :: !Text
    , dnsName          :: !Text
    , versionName      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

instance Hashable Names

instance ToJSON Names where
    toJSON = genericToJSON $ defaultOptions
        { fieldLabelModifier =
            Text.unpack . (`mappend` "_name") . stripText "Name" . Text.pack
        }

createNames :: Role -> Env -> Maybe Version -> Names
createNames (_role -> role) (_env -> env) ver = Names
    { envName          = env
    , keyName          = env <> "-khan"
    , roleName         = role
    , profileName      = envRole
    , groupName        = envRole
    , imageName        = roleVer
    , appName          = env <> "-" <> roleVer
    , balancerBaseName = envRole <> version '-' alphaVersion
    , dnsName          = Text.replace "_" "-" envRole
    , versionName      = Ver.toText <$> ver
    }
  where
    envRole = env <> "-" <> role
    roleVer = role <> Text.map f (version '_' Ver.toText)
      where
        f '+' = '/'
        f  c  = c

    version c f = maybe "" (Text.cons c) $ f <$> ver

unversioned :: Role -> Env -> Names
unversioned role env = createNames role env Nothing

versioned :: Role -> Env -> Version -> Names
versioned role env = createNames role env . Just

alphaVersion :: Version -> Text
alphaVersion = LText.toStrict . Build.toLazyText . Delim.toBuilder alpha
  where
    alpha = Delim.semantic
        & Delim.minor      .~ 'm'
        & Delim.patch      .~ 'p'
        & Delim.release    .~ 'r'
        & Delim.metadata   .~ 'b'
        & Delim.identifier .~ 'i'

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

newtype TrustPath = TrustPath { _trust :: FilePath }
    deriving (Eq, Show, IsString)

newtype PolicyPath = PolicyPath { _policy :: FilePath }
    deriving (Eq, Show, IsString)

newtype Env = Env { _env :: Text }
    deriving (Eq, Ord, Show, Invalid)

instance IsString Env where
    fromString = newEnv . fromString

instance ToJSON Env where
    toJSON = toJSON . _env

newEnv :: Text -> Env
newEnv = Env

newtype Role = Role { _role :: Text }
    deriving (Eq, Ord, Show, Invalid)

instance IsString Role where
    fromString = newRole . fromString

instance ToJSON Role where
    toJSON = toJSON . _role

newRole :: Text -> Role
newRole = Role . Text.map f
  where
    f '-' = '_'
    f c   = c

regionToText :: Region -> Text
regionToText = Text.pack . show

stripText :: Text -> Text -> Text
stripText x y =
    let z = fromMaybe y (Text.stripPrefix x y)
     in fromMaybe z (Text.stripSuffix x z)
