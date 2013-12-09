{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Prelude
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Prelude
    (
    -- * Types
      ByteString
    , Text
    , FilePath
    , NonEmpty (..)

    -- * Monadic
    , forever
    , join
    , when
    , whenM
    , unless
    , unlessM
    , void
    , lift

    -- * Errors
    , sync
    , throwError

    -- * Environment
    , accessKey
    , secretKey

    -- * Defaults
    , defaultEnv
    , defaultUser
    , defaultCache

    -- * Logging
    , enableLogging
    , log
    , log_
    , debug
    , debug_

    -- * Lists
    , diff

    -- * Re-exported Modules
    , module Applicative
    , module Error
    , module MonadIO
    , module Maybe
    , module Monoid
    , module Prime
    ) where

import           Control.Applicative       as Applicative
import           Control.Error             as Error
import           Control.Monad             (forever, join, when, unless, void)
import           Control.Monad.Error       (MonadError, throwError)
import           Control.Monad.IO.Class    as MonadIO
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import           Data.List                 ((\\))
import           Data.List.NonEmpty        (NonEmpty(..))
import           Data.Maybe                as Maybe
import           Data.Monoid               as Monoid
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as Text
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.Lazy            as LText
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as Path
import           Network.AWS
import           Prelude.Prime             as Prime hiding (FilePath, error, log, writeFile)
import           Shelly                    (whenM, unlessM)
import qualified System.IO                 as IO
import           System.Log.Handler.Simple
import           System.Log.Logger

accessKey, secretKey :: String
accessKey = "ACCESS_KEY_ID"
secretKey = "SECRET_ACCESS_KEY"

defaultEnv :: Text
defaultEnv = "dev"

defaultUser :: Text
defaultUser = "ubuntu"

defaultCache :: Int
defaultCache = 360

sync :: MonadIO m => IO a -> EitherT String m a
sync = fmapLT show . syncIO

enableLogging :: MonadIO m => m ()
enableLogging = liftIO $ do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering
    removeAllHandlers
    hd <- streamHandler IO.stdout INFO
    updateGlobalLogger logName (setLevel INFO . setHandlers [hd])

log :: (MonadIO m, Params ps) => Format -> ps -> m ()
log f = liftIO . infoM logName . LText.unpack . format f

log_ :: MonadIO m => Text -> m ()
log_ = log "{}" . Only

debug :: Params ps => Format -> ps -> AWS ()
debug f = whenDebug . log f

debug_ :: Text -> AWS ()
debug_ = whenDebug . log_

logName :: String
logName = "log"

diff :: Eq a => [a] -> [a] -> ([a], [a])
diff xs ys = (xs \\ ys, ys \\ xs)
