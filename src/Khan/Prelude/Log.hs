{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Prelude.Log
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Prelude.Log
   ( Color (..)
   , enableLogging
   , say
   , log
   , log_
   , debug
   , debug_
   ) where

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.IORef
import           Data.Text                    (Text)
import           Data.Text.Buildable          as Build
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as LText
import qualified Data.Text.Lazy.IO            as LText
import           Network.AWS                  (AWS, whenDebug)
import           Prelude                      hiding (log)
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafePerformIO)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

data Color where
    Red   :: Pretty a => a -> Color
    Green :: Pretty a => a -> Color
    Bold  :: Pretty a => a -> Color

instance Pretty Color where
    pretty (Red   x) = red   (pretty x)
    pretty (Green x) = green (pretty x)
    pretty (Bold  x) = bold  (pretty x)

    prettyList cs = "penis " PP.<$> indent 2 (vsep $ map item cs)
      where
        item x = char '-' <+> pretty x

instance Buildable Color where
    build = LText.fromString . show . pretty

logger :: IORef (LText.Text -> IO ())
logger = unsafePerformIO . newIORef . const $ return ()
{-# NOINLINE logger #-}

withLogger :: MonadIO m => LText.Text -> m ()
withLogger txt = liftIO $ readIORef logger >>= ($ txt)

enableLogging :: MonadIO m => m ()
enableLogging = liftIO $ do
    IO.hSetBuffering hd IO.LineBuffering
    atomicWriteIORef logger $! LText.hPutStrLn hd
  where
    hd = IO.stdout

say :: (MonadIO m, Pretty a) => Format -> [a] -> m ()
say f = withLogger . format f . map Bold

log :: (MonadIO m, Params ps) => Format -> ps -> m ()
log f = withLogger . format f

log_ :: MonadIO m => Text -> m ()
log_ = log "{}" . Only

debug :: Params ps => Format -> ps -> AWS ()
debug f = whenDebug . log f

debug_ :: Text -> AWS ()
debug_ = whenDebug . log_
