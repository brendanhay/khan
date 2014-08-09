{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Internal.IO
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.IO
    (
    -- * Shell
      sync

    -- * Concurrency
    , delaySeconds

    -- * Files
    , defaultPath
    , writeFile
    , setFileMode

    -- * Psuedo Randomisation
    , randomSelect
    , randomShuffle


    -- * Templates
    , renderTemplate
    ) where

import           Control.Concurrent        (threadDelay)
import           Data.Aeson                (Object)
import qualified Data.List                 as List
import           Data.Ord
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS               (AWS, liftEitherT)
import qualified System.Posix.Files        as Posix
import           System.Posix.Types        (FileMode)
import qualified System.Random             as Random
import qualified Text.EDE                  as EDE

sync :: IO a -> EitherT String IO a
sync = fmapLT show . syncIO

delaySeconds :: MonadIO m => Int -> m ()
delaySeconds n = liftIO $ threadDelay (n * 1000000)

defaultPath :: FilePath -> FilePath -> FilePath
defaultPath p def
    | invalid p = def
    | otherwise = p

writeFile :: MonadIO m => FilePath -> FileMode -> Text -> m ()
writeFile f m txt = liftIO $ do
    backup f
    FS.createTree (Path.parent f)
    FS.writeTextFile f txt
    setFileMode f m
  where
    backup p = do
        e <- FS.isFile p
        when e (unique p >>= FS.copyFile p)

    unique p = Path.addExtension p . name <$> getPOSIXTime
    name  ts = Text.pack (show (truncate ts :: Integer))

setFileMode :: MonadIO m => FilePath -> FileMode -> m ()
setFileMode f = liftIO . Posix.setFileMode (Path.encodeString f)

randomSelect :: MonadIO m => [a] -> m a
randomSelect xs = liftIO $ (xs !!) <$> Random.randomRIO (0, length xs - 1)

randomShuffle :: MonadIO m => [a] -> m [a]
randomShuffle xs = liftIO $
    map snd . List.sortBy (comparing fst) <$> Random.getStdRandom rs
  where
    rs g = foldl f ([], g) xs

    f (ys, g1) x =
        (\(n :: Int, g2) -> ((n, x) : ys, g2))
        (Random.random g1)

renderTemplate :: Object -> FilePath -> AWS LText.Text
renderTemplate o (Path.encodeString -> f) = liftEitherT $ do
    et <- sync (EDE.eitherParseFile f)
    hoistEither . join $ (`EDE.eitherRender` o) <$> et
