{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import qualified Filesystem                   as FS
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Internal                hiding (action)
import           Khan.Prelude
import           Network.HTTP.Conduit         hiding (path)
import           Options.Applicative          (execParser, info)
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..))

base :: String
base = "http://instance-data/"

data Action
    = File { _action :: Text }
    | Dir  { _action :: Text }

instance Semi.Semigroup Action where
    (<>) a b = bool Dir File (file b) $ strip (_action a) <> "/" <> _action b

instance Pretty Action where
    pretty = pretty . _action

action :: Text -> Action
action t
    | "latest/meta-data" <- t = Dir  "latest/meta-data/"
    | "latest/dynamic"   <- t = Dir  "latest/dynamic/"
    | "/" `Text.isSuffixOf` t = Dir  (strip t)
    | otherwise               = File t

url :: Action -> String
url = mappend base . Text.unpack . _action

path :: Action -> FilePath
path a = bool (`Path.addExtension` "list") id (file a) $
    Path.fromText (strip (_action a))

parent :: Action -> FilePath
parent = Path.directory . Path.fromText . _action

file :: Action -> Bool
file File{} = True
file _      = False

strip :: Text -> Text
strip x = fromMaybe x ("/" `Text.stripSuffix` x)

main :: IO ()
main = do
    !d <- execParser (info (options <**> helper) idm)

    enableLogging

    log_ "Checking if running on an EC2 instance ..."
    !_ <- simpleHttp base

    say "Ensuring {} exists ..." [d]
    FS.createTree d

    say "Changing working directory to {} ..." [d]
    FS.setWorkingDirectory d

    log_ "Starting metadata sync ..."
    withManager (`retrieve` action "latest/")
  where
    options = pathOption "dir"
        ( value "./instance-data"
       <> short 'd'
        ) "Path to output the synced metadata."

    retrieve m a = do
        say "Retrieving {} ..." [a]
        rq   <- parseUrl (url a)
        !txt <- strict . responseBody <$>
            httpLbs (rq { checkStatus = \ _ _ _ -> Nothing }) m

        lift $ FS.createTree (parent a)
            >> write (path a) txt

        unless (file a) $
            mapM_ (retrieve m . (a Semi.<>) . action)
                  (Text.lines txt)

    strict !lbs = LText.toStrict (LText.decodeUtf8 lbs)

    write p txt = do
        say "Writing {} ..." [p]
        FS.writeTextFile p txt

-- FIXME: use temporary directory and tar stuff up
