{-# LANGUAGE NoImplicitPrelude   #-}{-# LANGUAGE NoImplicitPrelude   #-}{-# LANGUAGE NoImplicitPrelude   #-}{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.SSH
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.SSH where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Time
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude            as P
import           System.Exit
import           System.Process
import           System.Timeout
import           Text.Show.Pretty

data Task = Task
    { taskHost :: !String
    , taskCmd  :: [String]
    } deriving Show

data Result = Result
    { resHost    :: !String
    , resTime    :: !NominalDiffTime
    , resPayload :: Either String String
    } deriving Show

-- runTasks :: [Task] -> Int -> Producer Result IO ()
-- runTasks tasks n = do
--      (output, input) <- lift $ spawn Unbounded
--      lift $ mapM_ (fork output) tasks
--      fromInput input
--   where
--     fork buf t = link =<< async (do
--         runEffect $ lift (runTask t n) >~ toOutput buf
--         performGC)

runTask :: Task -> Int -> IO Result
runTask Task{..} n = do
    (ts, res) <- time . timeout (n * 1000000) $
        readProcessWithExitCode "ssh" args []
    return . Result taskHost ts $ maybe (Left "Time out") result res
  where
    args = "-T" : taskHost : taskCmd

    result (ExitSuccess, out, _)   = Right out
    result (ExitFailure _, _, err) = Left err

    time io = do
        t1 <- getCurrentTime
        !a <- io
        t2 <- getCurrentTime
        return (diffUTCTime t2 t1, a)
