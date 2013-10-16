{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    -- * Variadic Formatters
      logInfo
    , logError
    , logDebug

    -- * String-Like
    , logInfo_
    , logError_
    , logDebug_

    -- * Re-exported
    , Shown (..)
    ) where

import qualified Data.Text               as Text
import           Data.Text.Buildable
import           Data.Text.Format        hiding (build)
import           Data.Text.Format.Params
import           Data.Text.IO            (hPutStrLn)
import qualified Data.Text.Lazy          as LText
import           Khan.Prelude
import           Network.AWS
import           System.IO               (stdout, stderr)

instance Buildable [Text] where
    build = build . Text.intercalate ", "

instance Buildable [LText.Text] where
    build = build . LText.intercalate ", "

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
