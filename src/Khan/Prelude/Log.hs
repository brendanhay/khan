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
   ( enableLogging
   , pp
   , log
   , log_
   , debug
   , debug_
   ) where

import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.IORef
import           Data.Text                    (Text)
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.IO            as LText
import           Network.AWS                  (AWS, whenDebug)
import           Prelude                      hiding (log)
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafePerformIO)
import           Text.PrettyPrint.Leijen.Text

logger :: IORef (LText.Text -> IO ())
logger = unsafePerformIO . newIORef . const $ return ()
{-# NOINLINE logger #-}

enableLogging :: MonadIO m => m ()
enableLogging = liftIO $ do
    IO.hSetBuffering hd IO.LineBuffering
    atomicWriteIORef logger $! LText.hPutStrLn hd
  where
    hd = IO.stdout

pp :: (MonadIO m, Pretty a) => a -> m ()
pp = log "{}" . Only . displayT . renderPretty 0.4 100 . pretty

log :: (MonadIO m, Params ps) => Format -> ps -> m ()
log f ps = liftIO $ readIORef logger >>= ($ format f ps)

log_ :: MonadIO m => Text -> m ()
log_ = log "{}" . Only

debug :: Params ps => Format -> ps -> AWS ()
debug f = whenDebug . log f

debug_ :: Text -> AWS ()
debug_ = whenDebug . log_
