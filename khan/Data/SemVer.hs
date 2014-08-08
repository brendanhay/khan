{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Data.SemVer
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.SemVer
    ( Version (versionRelease, versionMeta)

    , versionMajor
    , versionMinor
    , versionPatch

    , bumpMajor
    , bumpMinor
    , bumpPatch

    , newVersion
    , showVersion
    , alphaVersion
    , parseVersion

    , parseFileName
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char                  (isDigit)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Buildable
import qualified Data.Text.Lazy             as LText
import           Data.Text.Lazy.Builder     (Builder)
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import qualified Filesystem.Path.CurrentOS  as Path
import           Prelude                    hiding (takeWhile)

data Sep = Sep
    { sepMinor   :: !Builder
    , sepPatch   :: !Builder
    , sepRelease :: !Builder
    , sepMeta    :: !Builder
    }

data Version = Version
    { versionMajor   :: !Int
    , versionMinor   :: !Int
    , versionPatch   :: !Int
    , versionRelease :: Maybe Text
    , versionMeta    :: Maybe Text
    } deriving (Eq, Read, Show)

instance Ord Version where
    a `compare` b = branch a `compare` branch b
      where
        branch Version{..} =
            ( [versionMajor, versionMinor, versionPatch]
            , versionRelease
            , versionMeta
            )

instance ToJSON Version where
    toJSON = toJSON . showVersion

instance Buildable Version where
    build = build . (sep,)
      where
        sep = Sep
            { sepMinor   = Build.singleton '.'
            , sepPatch   = Build.singleton '.'
            , sepRelease = Build.singleton '-'
            , sepMeta    = Build.singleton '+'
            }

instance Buildable (Sep, Version) where
    build (Sep{..}, Version{..}) = mconcat
        [ Build.decimal versionMajor
        , sepMinor
        , Build.decimal versionMinor
        , sepPatch
        , Build.decimal versionPatch
        , f sepRelease versionRelease
        , f sepMeta versionMeta
        ]
      where
        f b = maybe mempty (mappend b . Build.fromText)

newtype Alpha = Alpha { unAlpha :: Version }

instance Buildable Alpha where
    build = build . (sep,) . unAlpha
      where
        sep = Sep
            { sepMinor   = Build.singleton 'm'
            , sepPatch   = Build.singleton 'p'
            , sepRelease = Build.singleton 'r'
            , sepMeta    = Build.singleton 'b'
            }

bumpMajor :: Version -> Version
bumpMajor v = v { versionMajor = versionMajor v + 1 }

bumpMinor :: Version -> Version
bumpMinor v = v { versionMinor = versionMinor v + 1 }

bumpPatch :: Version -> Version
bumpPatch v = v { versionPatch = versionPatch v + 1 }

newVersion :: Int -> Int -> Int -> Version
newVersion ma mi p = Version ma mi p Nothing Nothing

showVersion :: Version -> Text
showVersion = LText.toStrict . Build.toLazyText . build

alphaVersion :: Version -> Text
alphaVersion = LText.toStrict . Build.toLazyText . build . Alpha

parseVersion :: Text -> Either String Version
parseVersion = parseOnly version

parseFileName :: Path.FilePath -> Either String Version
parseFileName = f . Path.toText . Path.filename
  where
    f (Left  e) = Left $ Text.unpack e
    f (Right x) = parseOnly (manyTill anyChar (try end) >> version) x

    end = do
        void . satisfy $ inClass "- /_"
        p <- isDigit <$> peekChar'
        unless p $ fail ""

version :: Parser Version
version = Version
    <$> (decimal <* char '.')
    <*> (decimal <* char '.')
    <*> decimal
    <*> optional (try (char '-') *> takeWhile (/= '+'))
    <*> optional (try (char '+') *> takeWhile (/= '.'))
