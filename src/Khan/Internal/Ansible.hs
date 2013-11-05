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
import           Data.Aeson
import           Data.Aeson                       as Aeson
import qualified Data.Aeson.Encode.Pretty         as Aeson
import qualified Data.Attoparsec.Text             as T
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.IO                     as Text
import qualified Filesystem.Path.CurrentOS        as Path
import           Khan.Internal.AWS
import           Khan.Internal.IO
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import           System.Exit
import qualified Data.Text.Lazy as LText

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

capture :: Common -> AWS Output -> AWS ()
capture cmn aws = context cmn aws >>= exit . either failure id
  where
    failure (Err s) = Failed . format "{}" $ Only s
    failure (Ex ex) = failure . toError $ show ex

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

inventoryPath :: FilePath -> Text -> AWS FilePath
inventoryPath f env = do
    r <- Text.pack . show <$> getRegion
    defaultPath f
        . cachePath
        . Path.fromText
        $ Text.concat [r, "_", env]

data Inv a
    = Meta { unwrap :: a }
    | JS   { unwrap :: a }

deriving instance Eq  a => Eq (Inv a)
deriving instance Ord a => Ord (Inv a)

data Host = Host
    { hvFQDN   :: !Text
    , hvDomain :: !Text
    , hvNames  :: !Names
    , hvRegion :: !Text
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
        (Meta _) -> object vars
        (JS   _) -> pack hvFQDN
      where
        Host{..}  = unwrap x
        Names{..} = hvNames

        pack = Aeson.String

        vars = [ ("khan_region",  pack hvRegion)
               , ("khan_domain",  pack hvDomain)
               , ("khan_env",     pack envName)
               , ("khan_key",     pack keyName)
               , ("khan_role",    pack roleName)
               , ("khan_profile", pack profileName)
               , ("khan_group",   pack groupName)
               , ("khan_image",   pack imageName)
               , ("khan_app",     pack appName)
               , ("khan_version", toJSON versionName)
               ]
