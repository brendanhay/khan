{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
    , path
    , defaultPath
    , keyPath
    , cachePath
    , configPath
    , expandPath
    , writeFile
    , readConfig

    -- * Psuedo Randomisation
    , shuffle

    -- Re-exported
    , Sh
    , (</>)
    , (<.>)
    ) where

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.String
import qualified Data.Text                  as Text
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import           Shelly                     (Sh, (</>), (<.>), absPath, shellyNoDir, toTextIgnore)
import qualified Shelly                     as Shell
import           System.Directory
import qualified System.Environment         as Env
import           System.Random              (randomRIO)

sh :: MonadIO m => Sh a -> EitherT String m a
sh = fmapLT show . syncIO . shell

shell :: MonadIO m => Sh a -> m a
shell = shellyNoDir

path :: FilePath -> Text
path = toTextIgnore

defaultPath :: (Functor m, MonadIO m) => FilePath -> m FilePath -> m FilePath
defaultPath p def
    | invalid p = def
    | otherwise = return p

keyPath :: Names -> AWS FilePath
keyPath Names{..} = do
    r <- Text.pack . show <$> getRegion
    certPath . Shell.fromText $ Text.concat [r, "_", keyName, ".pem"]

certPath :: (Functor m, MonadIO m) => FilePath -> m FilePath
certPath f = (</> f) <$> ensurePath True "/etc/ssl/khan" "certs"

cachePath :: (Functor m, MonadIO m) => FilePath -> m FilePath
cachePath f = (</> f) <$> ensurePath True "/var/cache/khan" "cache"

configPath :: (Functor m, MonadIO m) => FilePath -> m FilePath
configPath f = (</> f) <$> ensurePath False "/etc/khan" "config"

ensurePath :: MonadIO m => Bool -> FilePath -> FilePath -> m FilePath
ensurePath parent dir sub = liftIO $ do
    p <- doesDirectoryExist $ Path.encodeString full
    f <- if p
             then return dir
             else format <$> Env.getExecutablePath
    createDirectoryIfMissing False $ Path.encodeString f
    expandPath f
  where
    full = if parent then Path.parent dir else dir

    format = (</> sub) . Path.parent . Path.decodeString

expandPath :: (Functor m, MonadIO m) => FilePath -> m FilePath
expandPath f =
    case "~/" `Text.stripPrefix` path f of
        Nothing -> return f
        Just x  -> do
            h <- liftIO getHomeDirectory
            shell . absPath $ h </> Path.fromText x

writeFile :: (Functor m, MonadIO m) => FilePath -> Text -> Text -> m ()
writeFile file mode contents = shell $ do
    backup file
    Shell.mkdir_p $ Path.parent file
    Shell.writefile file contents
    Shell.run_ "chmod" [mode, path file]
  where
    backup f = Shell.unlessM (Shell.test_e f) $ do
        ts <- liftIO (truncate <$> getPOSIXTime :: IO Integer)
        Shell.mv f $ f <.> Text.pack (show ts)
        backup f

readConfig :: (Functor m, MonadIO m) => FilePath -> m ByteString
readConfig f = configPath f >>= shell . Shell.readBinary

shuffle :: MonadIO m => [a] -> m a
shuffle xs = liftIO $ (xs !!) <$> randomRIO (0, length xs - 1)
