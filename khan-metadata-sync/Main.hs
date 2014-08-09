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
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Encoding      as LText
import qualified Filesystem                   as FS
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Internal                hiding (action)
import           Khan.Prelude
import           Network.HTTP.Conduit         (simpleHttp)
import           Options.Applicative          (execParser, info)
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..))

data Action
    = File { _action :: Text }
    | Dir  { _action :: Text }

instance Semi.Semigroup Action where
    (<>) a b = bool Dir File (file b) $ strip (_action a) <> "/" <> _action b

instance Pretty Action where
    pretty = pretty . _action

action :: Text -> Action
action t
    | "/" `Text.isSuffixOf` t = Dir  (strip t)
    | otherwise               = File t

url :: Action -> String
url = mappend "http://169.254.169.254/" . Text.unpack . _action

path :: Action -> FilePath
path a = bool (`Path.addExtension` "list") id (file a) $
    Path.fromText (strip (_action a))

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
    !_ <- simpleHttp "http://instance-data/"

    say "Ensuring {} exists ..." [d]
    FS.createTree d

    say "Changing working directory to {} ..." [d]
    FS.setWorkingDirectory d

    log_ "Starting metadata sync ..."
    retrieve (action "latest/")
  where
    options = pathOption "dir"
        ( value "./instance-data"
       <> short 'd'
        ) "Path to output the synced metadata."

    retrieve a = do
        say "Retrieving {} ..." [a]
        !txt <- strict <$> simpleHttp (url a)
        write a txt
        unless (file a) $
            mapM_ (retrieve . (a Semi.<>) . action)
                  (Text.lines txt)

    write (path -> p) txt = do
        say "Writing {} ..." [p]
        FS.writeTextFile p txt

    strict !lbs = LText.toStrict (LText.decodeUtf8 lbs)
