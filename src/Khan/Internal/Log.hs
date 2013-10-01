{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Internal.Log
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Log
    (
    -- * Variadic AWS Errors
      throwErrorF
    , noteErrorF

    -- * Variadic Formatters
    , logInfo
    , logError
    , logDebug

    -- * String-Like
    , logInfo_
    , logError_
    , logDebug_

    -- * Re-exported
    , Shown (..)
    ) where

import Control.Monad.IO.Class
import Data.Monoid
import Data.String
import Data.Text               (Text)
import Data.Text.Format
import Data.Text.Format.Params
import Data.Text.IO            (hPutStrLn)
import Data.Text.Lazy          (unpack)
import Network.AWS
import System.IO               (stdout, stderr)

throwErrorF :: Params ps => Format -> ps -> AWS a
throwErrorF f = throwError . unpack . format f

-- noteErrorF :: Params ps => Format -> ps -> AWS a
noteErrorF f ps = noteError (unpack $ format f ps)

logInfo, logError :: (MonadIO m, Params ps) => Format -> ps -> m ()
logInfo  f = hprint stdout (f <> "\n")
logError f = hprint stderr (f <> "\n")

logInfo_, logError_ :: MonadIO m => Text -> m ()
logInfo_  = liftIO . hPutStrLn stdout
logError_ = liftIO . hPutStrLn stderr

logDebug :: Params ps => Format -> ps -> AWS ()
logDebug fmt = whenDebug . logInfo fmt

logDebug_ :: Text -> AWS ()
logDebug_ = whenDebug . logInfo_
