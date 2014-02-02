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
      sh
    , shell

    -- * Files
    , defaultPath
    , expandPath
    , writeFile

    -- * Psuedo Randomisation
    , randomSelect
    , randomShuffle

    -- * Templates
    , renderTemplate

    -- Re-exported
    , Sh
    , (</>)
    , (<.>)
    ) where

import           Data.Aeson                (Object)
import qualified Data.List                 as List
import           Data.Ord
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS               (AWS, liftEitherT)
import           Shelly                    (Sh, (</>), (<.>), absPath, shellyNoDir, toTextIgnore)
import qualified Shelly                    as Shell
import           System.Directory
import qualified System.Random             as Random
import qualified Text.EDE                  as EDE

sh :: MonadIO m => Sh a -> EitherT String m a
sh = fmapLT show . syncIO . shell

shell :: MonadIO m => Sh a -> m a
shell = shellyNoDir

defaultPath :: FilePath -> FilePath -> FilePath
defaultPath p def
    | invalid p = def
    | otherwise = p

expandPath :: (Functor m, MonadIO m) => FilePath -> m FilePath
expandPath f =
    case "~/" `Text.stripPrefix` toTextIgnore f of
        Nothing -> return f
        Just x  -> do
            h <- liftIO getHomeDirectory
            shell . absPath $ h </> Path.fromText x

writeFile :: (Functor m, MonadIO m) => FilePath -> Text -> Text -> m ()
writeFile file mode contents = shell $ do
    backup file
    Shell.mkdir_p $ Path.parent file
    Shell.writefile file contents
    Shell.run_ "chmod" [mode, toTextIgnore file]
  where
    backup f = Shell.unlessM (Shell.test_e f) $ do
        ts <- liftIO (truncate <$> getPOSIXTime :: IO Integer)
        Shell.mv f $ f <.> Text.pack (show ts)
        backup f

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
    et <- sync $ EDE.eitherParseFile f
    hoistEither . join $ (`EDE.eitherRender` o) <$> et
