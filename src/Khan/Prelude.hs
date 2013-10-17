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

    -- * Errors
    , sync
    , assert
    , throwFormat
    , noteFormat
    , throwError

    -- * Caba Data Files
    , defaultDataFile
    , dataFile

    -- * Defaults
    , defaultKeyPath
    , defaultTmpPath
    , defaultEnv
    , defaultVersion

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
import Control.Monad.Error       (MonadError, Error, throwError)
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
import Data.Version
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
    )

sh :: MonadIO m => Sh a -> EitherT String m a
sh = fmapLT show . syncIO . shell

shell :: MonadIO m => Sh a -> m a
shell = shellyNoDir

path :: FilePath -> Text
path = toTextIgnore

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

defaultDataFile :: (Functor m, MonadIO m) => FilePath -> String -> m FilePath
defaultDataFile f name
    | mempty /= f = return f
    | otherwise   = dataFile name

dataFile :: (Functor m, MonadIO m) => String -> m FilePath
dataFile name = liftIO (getDataFileName name) >>= shell . absPath . fromString

defaultKeyPath :: FilePath
defaultKeyPath = "~/.khan/keys"

defaultTmpPath :: FilePath
defaultTmpPath = ".khan"

defaultEnv :: Text
defaultEnv = "dev"

defaultVersion :: Version
defaultVersion = Version [0] []
