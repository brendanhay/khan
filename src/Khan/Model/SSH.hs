{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Model.SSH
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.SSH
     ( exec
     , wait
     ) where

import           Control.Concurrent        (threadDelay)
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude
import qualified Shelly                    as Shell
import qualified System.Posix.Process      as Posix

exec :: MonadIO m => Text -> Text -> FilePath -> [String] -> m a
exec addr user key xs = liftIO $ do
    log "ssh {}" [unwords as]
    Posix.executeFile "ssh" True as Nothing
  where
    as = args addr user key xs

wait :: MonadIO m => Int -> Text -> Text -> FilePath -> m Bool
wait s addr user key = do
    log "Waiting {} seconds for SSH on {}" [show s, Text.unpack addr]
    liftIO $ go (s * 1000000)
  where
    go n
        | n <= 0    = return False
        | otherwise = do
            log_ "Waiting..." *> threadDelay delay
            e <- runEitherT . sh . Shell.silently $ Shell.run "ssh" xs
            either (const . go $ n - delay)
                   (return . const True)
                   e

    delay = 20 * 1000000

    xs = map Text.pack $ args addr user key ["-q", "-o", "BatchMode=yes"]

args :: Text -> Text -> FilePath -> [String] -> [String]
args addr user key = mappend
    [ "-i" ++ Path.encodeString key
    , Text.unpack $ user <> "@" <> addr
    ]
