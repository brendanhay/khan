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

    -- * Templates
    , render

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
import           System.Random              (randomRIO)
import           Text.Hastache
import           Text.Hastache.Aeson

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
    d   <- configPath "keys" >>= expandPath
    reg <- Text.pack . show <$> getRegion
    return . (d </>) . Shell.fromText $ Text.concat [reg, "_", keyName, ".pem"]

cachePath :: (Functor m, MonadIO m) => FilePath -> m FilePath
cachePath f = (</> f) <$> ensurePath True "/var/cache/khan" "cache"

configPath :: (Functor m, MonadIO m) => FilePath -> m FilePath
configPath f = (</> f) <$> ensurePath False "/etc/khan" "config"

ensurePath :: MonadIO m => Bool -> FilePath -> FilePath -> m FilePath
ensurePath parent dir sub = liftIO $ do
    p <- doesDirectoryExist $ Path.encodeString d
    f <- if p
             then return dir
             else (</> sub) . Path.decodeString <$> getCurrentDirectory
    createDirectoryIfMissing False $ Path.encodeString f
    return f
  where
    d = if parent then Path.parent dir else dir

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

render :: (Functor m, MonadIO m) => FilePath -> Aeson.Value -> m LBS.ByteString
render f x = do
    t <- readTemplate f
    hastacheStr defaultConfig t $ jsonValueContext x

readTemplate :: (Functor m, MonadIO m) => FilePath -> m ByteString
readTemplate f = configPath f >>= shell . Shell.readBinary

shuffle :: MonadIO m => [a] -> m a
shuffle xs = liftIO $ (xs !!) <$> randomRIO (0, length xs - 1)
