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
    , Map
    , Text
    , FilePath

    -- * Monadic
    , forever
    , join
    , when
    , unless
    , void
    , lift

    -- * Exceptions
    , sync

    -- * Errors
    , assertFormat
    , throwFormat
    , noteFormat
    , throwError

    -- * Variadic Loggers
    , log
    , error
    , debug

    -- * Non-variadic Loggers
    , log_
    , error_
    , debug_

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
import           Data.Map                  (Map)
import           Data.Maybe                as Maybe
import           Data.Monoid               as Monoid
import           Data.String
import           Data.Text                 (Text)
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.IO              as Text
import           Data.Text.Lazy            (unpack)
import           Filesystem.Path.CurrentOS (FilePath)
import           Network.AWS
import           Prelude.Prime             as Prime hiding (FilePath, error, log, writeFile)
import qualified System.IO                 as IO

sync :: MonadIO m => IO a -> EitherT String m a
sync = fmapLT show . syncIO

assertFormat :: (MonadError AWSError m, MonadIO m, Params ps)
       => Format
       -> ps
       -> Bool
       -> m ()
assertFormat f ps True  = throwFormat f ps
assertFormat _ _  False = return ()

throwFormat :: (Params a, MonadError AWSError m) => Format -> a -> m b
throwFormat f = throwError . Err . unpack . format f

noteFormat :: (Params ps, MonadError AWSError m) => Format -> ps -> Maybe a -> m a
noteFormat f ps = hoistError . note (Err . unpack $ format f ps)

log, error :: (MonadIO m, Params ps) => Format -> ps -> m ()
log f   = hprint IO.stdout (f <> "\n")
error f = hprint IO.stderr (f <> "\n")

log_, error_ :: MonadIO m => Text -> m ()
log_   = liftIO . Text.putStrLn
error_ = liftIO . Text.hPutStrLn IO.stderr

debug :: Params ps => Format -> ps -> AWS ()
debug fmt = whenDebug . log fmt

debug_ :: Text -> AWS ()
debug_ = whenDebug . log_
