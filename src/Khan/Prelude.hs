{-# LANGUAGE NoImplicitPrelude #-}

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
    , Sh

    , (</>)

    , forever
    , join
    , when
    , unless
    , void
    , lift

    -- * Shell
    , sh
    , shell
    , path

    -- * Variadic AWS Errors
    , throwErrorF
    , noteErrorF

    -- * Variadic EitherT Errors
    , assert

    -- * Caba Data Files
    , defaultDataFile
    , dataFile

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
import Network.AWS
import Paths_khan                (getDataFileName)
import Prelude.Prime             as Prime hiding (FilePath)

import Shelly
    ( FilePath
    , Sh
    , (</>)
    , absPath
    , shellyNoDir
    , toTextIgnore
    , verbosely
    )

sh :: MonadIO m => Sh a -> EitherT String m a
sh = fmapLT show . syncIO . shell

shell :: MonadIO m => Sh a -> m a
shell = shellyNoDir . verbosely

path :: FilePath -> Text
path = toTextIgnore

assert :: (MonadIO m, Params ps) => Format -> ps -> Bool -> EitherT String m ()
assert f ps True  = left . unpack $ format f ps
assert _ _  False = right ()

throwErrorF :: Params ps => Format -> ps -> AWS a
throwErrorF f = throwError . unpack . format f

noteErrorF :: Params ps => Format -> ps -> Maybe a -> AWS a
noteErrorF f ps = noteError (unpack $ format f ps)

defaultDataFile :: (Functor m, MonadIO m) => FilePath -> String -> m FilePath
defaultDataFile f name
    | mempty /= f = return f
    | otherwise   = dataFile name

dataFile :: (Functor m, MonadIO m) => String -> m FilePath
dataFile name = liftIO (getDataFileName name) >>= shell . absPath . fromString
