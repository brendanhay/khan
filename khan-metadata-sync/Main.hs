{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Main
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import qualified Data.Semigroup               as Semi
import           Data.String
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import qualified Filesystem                   as FS
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Internal                hiding (action)
import           Khan.Prelude
import           Network.HTTP.Conduit         hiding (path)
import           System.IO.Temp               (withSystemTempDirectory)
import           System.Process
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..))

base :: String
base = "http://instance-data/"

output :: Text
output = "./metadata.tar.gz"

data Action
    = File { _action :: Text }
    | Dir  { _action :: Text }

instance Semi.Semigroup Action where
    (<>) a b = bool Dir File (isFile b) $ strip (_action a) <> "/" <> _action b

instance Pretty Action where
    pretty = pretty . _action

instance IsString Action where
    fromString = action . Text.pack

action :: Text -> Action
action (Text.strip -> t)
    | "/" `Text.isSuffixOf` t = Dir  t
    | otherwise               = File t

url :: Action -> String
url = mappend base . Text.unpack . _action

path :: Action -> FilePath
path a = bool (`Path.addExtension` "list") id (isFile a) $
    Path.fromText (strip (_action a))

isFile :: Action -> Bool
isFile File{} = True
isFile _      = False

strip :: Text -> Text
strip x = fromMaybe x ("/" `Text.stripSuffix` x)

main :: IO ()
main = do
    enableLogging

    log_ "Checking if running on an EC2 instance ..."
    !_ <- simpleHttp base

    withSystemTempDirectory "metadata." $ \(Path.decodeString -> d) -> do
        log_ "Starting metadata synchronisation ..."
        withManager $ \m ->
            mapM_ (retrieve d m)
                [ "latest/meta-data/"
                , "latest/user-data/"
                , "latest/dynamic"
                ]

        say "Compressing {} ..." [output]
        readProcess "tar" (map Text.unpack ["-cvf", output, toTextIgnore d]) ""
            >>= mapM_ putStrLn . lines

    log_ "Completed."
  where
    retrieve d m a = do
        say "Retrieve {}" [a]
        rq <- parseUrl (url a)
        rs <- httpLbs (rq { checkStatus = \ _ _ _ -> Nothing }) m

        when (fromEnum (responseStatus rs) == 200) $ do
            let txt = strict (responseBody rs)
            write d a txt
            unless (isFile a) $
                mapM_ (retrieve d m . (a Semi.<>) . action)
                      (Text.lines txt)

    strict !lbs = LText.toStrict (LText.decodeUtf8 lbs)

    write d a txt = lift $
        let p = d </> path a
         in FS.createTree (Path.directory p)
         >> FS.writeTextFile p txt
