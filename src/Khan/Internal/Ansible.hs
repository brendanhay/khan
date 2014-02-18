{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

-- Module      : Khan.Internal.Ansible
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Ansible where

import           Control.Monad.Error
import           Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.Attoparsec.Text       as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (intercalate)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.IO               as Text
import qualified Data.Text.Lazy             as LText
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal.AWS
import           Khan.Internal.IO
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import           System.Exit

data Output
    = Changed !LText.Text
    | Unchanged !LText.Text
    | Failed !LText.Text
      deriving (Show)

instance ToJSON Output where
    toJSON (Changed   msg) = object ["changed" .= True,  "msg" .= msg]
    toJSON (Unchanged msg) = object ["changed" .= False, "msg" .= msg]
    toJSON (Failed    msg) = object ["failed"  .= True,  "msg" .= msg]

changed, unchanged :: (Monad m, Params ps) => Format -> ps -> m Output
changed   f = return . Changed . format f
unchanged f = return . Unchanged . format f

failed :: (Monad m, Params ps) => Format -> ps -> m Output
failed f = return . Failed . format f

capture :: Params ps => Common -> Format -> ps -> AWS Bool -> AWS ()
capture c f ps aws = capture' c $ aws >>= success
  where
    success True  = changed (f <> " changed.") ps
    success False = unchanged (f <> " unchanged.") ps

capture' :: Common -> AWS Output -> AWS ()
capture' c aws = contextAWS c aws
     >>= either failure (return . id)
     >>= exit
  where
    failure (Err s)  = failed "{}" $ Only s
    failure (Ex  ex) = failure . toError $ show ex
    failure (Ers es) = failure . toError . intercalate ", " $ map show es

    exit o = liftIO $ LBS.putStrLn (Aeson.encodePretty o) >>
        case o of
            Failed _ -> exitFailure
            _        -> exitSuccess

parseArgsFile :: FilePath -> AWS [(Text, Text)]
parseArgsFile path = do
    line <- liftEitherT . sync . Text.readFile $ Path.encodeString path
    hoistError . fmapL toError $ T.parseOnly kvs line
  where
    kvs = T.many1 $ (,)
        <$> (T.skipSpace *> T.takeWhile (/= '=') <* T.char '=')
        <*> (quoted '\'' <|> quoted '"' <|> spaced)

    quoted c = T.char c *> T.takeWhile (/= c) <* T.char c
    spaced   = T.takeWhile (not . end)

    end c = T.isHorizontalSpace c || T.isEndOfLine c

inventoryPath :: CacheDir -> Env -> AWS FilePath
inventoryPath (CacheDir dir) env = do
    r <- Text.pack . show <$> getRegion
    return $ dir </> Path.fromText (Text.concat [r, "_", _env env])

data Inv a
    = Meta { unwrap :: a }
    | JS   { unwrap :: a }

deriving instance Eq  a => Eq (Inv a)
deriving instance Ord a => Ord (Inv a)

data Host = Host
    { hvFQDN   :: !Text
    , hvDomain :: !Text
    , hvNames  :: !Names
    , hvRegion :: !Region
    } deriving (Eq, Ord)

instance ToJSON (Inv (HashMap Text (Set Host))) where
    toJSON (Meta m) = object ["_meta" .= object ["hostvars" .= vars]]
      where
        vars = foldl' (flip f) Map.empty . Set.unions $ Map.elems m
        f h  = Map.insert (hvFQDN h) (Meta h)

    toJSON (JS m) = toJSON (Map.map JS m) `f` toJSON (Meta m) `f` local
      where
        f (Object x) (Object y) = Object $ x <> y
        f _          x          = x

        local = object ["localhost" .= ["localhost" :: Text]]

instance ToJSON (Inv (Set Host)) where
    toJSON x = case x of
        (Meta _) -> f Meta
        (JS   _) -> f JS
      where
        f c = toJSON . map c . Set.toList $ unwrap x

instance ToJSON (Inv Host) where
    toJSON x = case x of
        (JS   _) -> String hvFQDN
        (Meta _) -> object $
            ("khan_domain", String hvDomain) : variables hvRegion hvNames
      where
        Host{..} = unwrap x

data ImageInput = ImageInput
    { iNames  :: !Names
    , iRegion :: !Region
    , iDNS    :: !Text
    }

instance ToJSON ImageInput where
    toJSON ImageInput{..} =
        object $ ("khan_dns", String iDNS) : variables iRegion iNames

variables :: Region -> Names -> [(Text, Value)]
variables reg Names{..} =
    [ ("khan_region",        String . Text.pack $ show reg)
    , ("khan_region_abbrev", String $ abbreviate reg)
    , ("khan_env",           String envName)
    , ("khan_key",           String keyName)
    , ("khan_role",          String roleName)
    , ("khan_profile",       String profileName)
    , ("khan_group",         String groupName)
    , ("khan_image",         String imageName)
    , ("khan_app",           String appName)
    , ("khan_version",       toJSON versionName)
    ]
