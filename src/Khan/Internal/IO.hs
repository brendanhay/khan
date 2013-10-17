{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
    , path
    , expandPath
    , writeFile

    -- * Caba Data Files
    , defaultDataFile
    , dataFile

    -- * metadata.rb
    , cookbookMeta

    -- * Psuedo Randomisation
    , shuffle

    -- Re-exported
    , Sh
    , (</>)
    , (<.>)
    ) where

import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import           Data.Time.Clock.POSIX
import           Filesystem.Path.CurrentOS (fromText, parent)
import           Khan.Internal.Log
import           Khan.Internal.Types
import           Khan.Prelude
import           Paths_khan                (getDataFileName)
import           Shelly                    (Sh, (</>), (<.>), absPath, shellyNoDir, toTextIgnore)
import qualified Shelly                    as Shell
import           System.Directory          (getHomeDirectory)
import           System.Random             (randomRIO)

sh :: MonadIO m => Sh a -> EitherT String m a
sh = fmapLT show . syncIO . shell

shell :: MonadIO m => Sh a -> m a
shell = shellyNoDir

path :: FilePath -> Text
path = toTextIgnore

expandPath :: (Functor m, MonadIO m) => FilePath -> m FilePath
expandPath f =
    case "~/" `Text.stripPrefix` path f of
        Nothing -> return f
        Just x  -> do
            h <- liftIO getHomeDirectory
            shell . absPath $ h </> fromText x

writeFile :: (Functor m, MonadIO m) => FilePath -> Text -> Text -> m ()
writeFile file mode contents = shell $ do
    backup file
    Shell.mkdir_p $ parent file
    Shell.writefile file contents
    Shell.run_ "chmod" [mode, path file]
  where
    backup f = Shell.unlessM (Shell.test_e f) $ do
        ts <- liftIO (truncate <$> getPOSIXTime :: IO Integer)
        Shell.mv f $ f <.> Text.pack (show ts)
        backup f

defaultDataFile :: (Functor m, MonadIO m) => FilePath -> String -> m FilePath
defaultDataFile f name
    | mempty /= f = return f
    | otherwise   = dataFile name

dataFile :: (Functor m, MonadIO m) => String -> m FilePath
dataFile name = liftIO (getDataFileName name) >>= shell . absPath . fromString

cookbookMeta :: MonadIO m => Text -> Text -> m Text
cookbookMeta def key
    | not $ invalid def = return def
    | otherwise         = do
        p <- shell $ Shell.test_e "metadata.rb"
        r <- liftIO $ extract p
        logInfo "Using role '{}'" [r]
        return r
  where
    extract False = return def
    extract True  = do
        logInfo_ "Reading metadata.rb"
        strip . fromMaybe def . split . Text.lines <$>
            Text.readFile "metadata.rb"

    split = fmap (Text.dropAround separator . Text.dropWhile (not . separator)) .
        find (key `Text.isPrefixOf`)

    separator c = c == '\'' || c == '"'

    strip s = fromMaybe s $ Text.stripSuffix "-role" s

shuffle :: MonadIO m => [a] -> m a
shuffle xs = liftIO $ (xs !!) <$> randomRIO (0, length xs - 1)
