{-# LANGUAGE RecordWildCards #-}

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
    ( Version (..)

    , bumpMajor
    , bumpMinor
    , bumpPatch

    , newVersion
    , showVersion
    , parseVersion
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import           Prelude                    hiding (takeWhile)

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

bumpMajor :: Version -> Version
bumpMajor v = v { versionMajor = versionMajor v + 1 }

bumpMinor :: Version -> Version
bumpMinor v = v { versionMinor = versionMinor v + 1 }

bumpPatch :: Version -> Version
bumpPatch v = v { versionPatch = versionPatch v + 1 }

newVersion :: Int -> Int -> Int -> Version
newVersion ma mi p = Version ma mi p Nothing Nothing

showVersion :: Version -> Text
showVersion Version{..} = LText.toStrict . Build.toLazyText $ mconcat
    [ Build.decimal versionMajor
    , Build.singleton '.'
    , Build.decimal versionMinor
    , Build.singleton '.'
    , Build.decimal versionPatch
    , f '-' versionRelease
    , f '+' versionMeta
    ]
  where
    f c = maybe mempty (mappend (Build.singleton c) . Build.fromText)

parseVersion :: Text -> Either String Version
parseVersion = parseOnly version
  where
    version = Version
        <$> (decimal <* char '.')
        <*> (decimal <* char '.')
        <*> decimal
        <*> optional (try (char '-') *> takeWhile (/= '+'))
        <*> optional (try (char '+') *> takeText)
