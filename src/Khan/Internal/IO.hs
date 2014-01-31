{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

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
    , shuffle

    -- * Templates
    , renderTemplate

    -- Re-exported
    , Sh
    , (</>)
    , (<.>)
    ) where

import           Data.Aeson                (Object)
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS               (AWS, liftEitherT, hoistError)
import           Shelly                    (Sh, (</>), (<.>), absPath, shellyNoDir, toTextIgnore)
import qualified Shelly                    as Shell
import           System.Directory
import           System.Random             (randomRIO)
import           Text.EDE                  (Template)
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

shuffle :: MonadIO m => [a] -> m a
shuffle xs = liftIO $ (xs !!) <$> randomRIO (0, length xs - 1)

renderTemplate :: Object -> FilePath -> AWS LText.Text
renderTemplate o (Path.encodeString -> f) = liftEitherT $ do
    et <- sync $ EDE.eitherParseFile f
    hoistEither . join $ (`EDE.eitherRender` o) <$> et
