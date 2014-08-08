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
    , (<=<)
    , (>=>)
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

    -- * Lists
    , diff

    -- * Formatting
    , formatUTC

    -- * Re-exported Modules
    , module Applicative
    , module Error
    , module MonadIO
    , module Maybe
    , module Monoid
    , module Prime
    , module Log
    ) where

import Control.Applicative       as Applicative
import Control.Error             as Error
import Control.Monad             ((<=<), (>=>), forever, join, when, unless, void)
import Control.Monad.Except       (throwError)
import Control.Monad.IO.Class    as MonadIO
import Control.Monad.Trans.Class (lift)
import Data.ByteString           (ByteString)
import Data.List                 ((\\))
import Data.List.NonEmpty        (NonEmpty(..))
import Data.Maybe                as Maybe
import Data.Monoid               as Monoid
import Data.Text                 (Text)
import Data.Time
import Filesystem.Path.CurrentOS (FilePath)
import Khan.Prelude.Log          as Log
import Prelude.Prime             as Prime hiding (FilePath, error, log, writeFile)
import Shelly                    (whenM, unlessM)
import System.Locale

accessKey, secretKey :: String
accessKey = "ACCESS_KEY_ID"
secretKey = "SECRET_ACCESS_KEY"

sync :: MonadIO m => IO a -> EitherT String m a
sync = fmapLT show . syncIO

diff :: Eq a => [a] -> [a] -> ([a], [a])
diff xs ys = (xs \\ ys, ys \\ xs)

formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%d.%m.%Y %X"
