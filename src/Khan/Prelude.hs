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
    ( ByteString
    , Map
    , Text
    , FilePath

    , forever
    , join
    , when
    , unless
    , void
    , lift

    -- * Errors
    , sync
    , assert
    , throwFormat
    , noteFormat
    , throwError

    -- * Re-exported Modules
    , module Applicative
    , module Error
    , module MonadIO
    , module Maybe
    , module Monoid
    , module Prime
    ) where

import Control.Applicative       as Applicative
import Control.Error             as Error
import Control.Monad             (forever, join, when, unless, void)
import Control.Monad.Error       (MonadError, throwError)
import Control.Monad.IO.Class    as MonadIO
import Control.Monad.Trans.Class (lift)
import Data.ByteString           (ByteString)
import Data.Map                  (Map)
import Data.Maybe                as Maybe
import Data.Monoid               as Monoid
import Data.String
import Data.Text                 (Text)
import Data.Text.Format          (Format, format)
import Data.Text.Format.Params
import Data.Text.Lazy            (unpack)
import Filesystem.Path.CurrentOS (FilePath)
import Network.AWS
import Prelude.Prime             as Prime hiding (FilePath, writeFile)

sync :: MonadIO m => IO a -> EitherT String m a
sync = fmapLT show . syncIO

assert :: (MonadError String m, MonadIO m, Params ps)
       => Format
       -> ps
       -> Bool
       -> m ()
assert f ps True  = throwError . unpack $ format f ps
assert _ _  False = return ()

throwFormat :: (Params a, MonadError AWSError m) => Format -> a -> m b
throwFormat f = throwError . Err . unpack . format f

noteFormat :: (Params ps, MonadError AWSError m) => Format -> ps -> Maybe a -> m a
noteFormat f ps = hoistError . note (Err . unpack $ format f ps)
