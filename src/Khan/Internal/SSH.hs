{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.SSH
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.SSH where

import           Control.Applicative
import           Control.Arrow          ((***))
import           Control.Concurrent     (threadDelay)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import           Data.List              (partition)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Khan.Group             as Group
import           Khan.Internal
import           Khan.Internal.Types    as Types
import           Network.AWS
import           Network.AWS.EC2
import           Text.Show.Pretty

data Task = Task
    { taskHost :: String
    , taskCmd  :: [String]
    } deriving Show

data Result = Result
    { resHost    :: String
    , resPayload :: Either String String
    , resTime    :: NominalDiffTime
    } deriving Show

-- | time the given IO action (clock time) and return a tuple
--   of the execution time and the result
timeIO :: IO a -> IO (NominalDiffTime, a)
timeIO ioa = do
    t1 <- getCurrentTime
    !a <- ioa
    t2 <- getCurrentTime
    return (diffUTCTime t2 t1, a)

runTasks :: [Task] -> Int -> (Result -> IO a) -> IO ()
runTasks ts tmout handler = do
    out <- newChan
    forM_ ts $ run out
    replicateM_ (length ts) (readChan out >>= handler)
  where
    run ch t = forkIO $ do
        !res <- runTask t tmout
        writeChan ch res

runTask :: Task -> Int -> IO Result
runTask Task{..} tmout = do
    (time, res) <- timeIO . timeout tmout $ readProcessWithExitCode "ssh" args []
    case res of
        Nothing -> output time $ Left "timed out\n"
        Just (code, stdout', stderr') -> 
          output time $ case code of
              ExitSuccess   -> Right stdout'
              ExitFailure _ -> Left stderr'
  where
    args = "-T" : taskHost : taskCmd

    output time payload = return $ Result taskHost payload time

mkTasks :: [String] -> [String] -> [Task]
mkTasks hosts cmd = map f hosts
  where
    f h = Task h cmd

----------------
-- PRINTING
----------------

putColorLn :: ColorIntensity -> Color -> String -> IO ()
putColorLn intensity color s = do
    setSGR [SetColor Foreground intensity color]
    putStrLn s
    setSGR []

printResult :: Result -> IO ()
printResult Result{..} = do
    putColorLn Dull Blue (resHost ++ t)
    case resPayload of
      Left s  -> putColorLn Vivid Red s
      Right s -> putStrLn s
    where t = printf " (%.1fs)" (realToFrac resTime :: Double) 

printShortResult :: Result -> IO ()
printShortResult Result{..} =
    case resPayload of
      Left s  -> putColorLn Vivid Red $ resHost ++ ": " ++ rstrip s
      Right s -> putStrLn $ resHost ++ ": " ++ rstrip s


----------------
-- ARGS
----------------

data Options = Options
    { oFile    :: FilePath
    , oCommand :: String
    , oTimeout :: Int
    , oShort   :: Bool
    } deriving Show

options :: Parser Options
options = Options <$> argument str (metavar "FILE")
                  <*> argument str (metavar "COMMAND")
                  <*> option
                      ( short 't'
                      <> metavar "TIMEOUT"
                      <> help "ssh timout in seconds"
                      <> showDefault
                      <> value 10 )
                  <*> switch 
                      ( short 's'
                      <> metavar "SHORT"
                      <> help "display results in short format" )

main :: IO ()
main = execParser (info (helper <*> options) fullDesc) >>= go

go :: Options -> IO ()
go Options{..} = do
    hosts <- readHosts
    runTasks (mkTasks hosts cmd) tmout handler
  where
    readHosts = filter ((not . null) . strip) . lines <$> readFile oFile
    handler   = if oShort then printShortResult else printResult
    tmout     = oTimeout * 1000000
    cmd       = words oCommand
