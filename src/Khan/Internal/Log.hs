-- Module      : Khan.Internal.Log
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Log where

import Control.Monad.IO.Class
import System.IO
import System.Log.Handler.Simple
import System.Log.Logger

logStep :: (MonadIO m, Show a) => String -> a -> m ()
logStep msg = (logInfo msg >>) . logDebug . show

logInfo, logWarn, logError, logDebug :: MonadIO m => String -> m ()
logInfo  = logMsg infoM
logWarn  = logMsg warningM
logError = logMsg errorM
logDebug = logMsg debugM

logMsg :: MonadIO m => (String -> a -> IO ()) -> a -> m ()
logMsg f = liftIO . f logName

logName :: String
logName = "log"

setLogging :: MonadIO m => Bool -> m ()
setLogging debug = liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    removeAllHandlers
    hd <- streamHandler stderr prio
    updateGlobalLogger logName (setLevel prio . setHandlers [hd])
  where
    prio = if debug then DEBUG else INFO
