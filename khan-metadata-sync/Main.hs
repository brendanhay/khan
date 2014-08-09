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

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Encoding   as LText
import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude
import           Network.HTTP.Conduit      (simpleHttp)
import           Options.Applicative
import           System.IO

main :: IO ()
main = do
    !d <- execParser (info (directory <**> helper) idm)

    enableLogging

    log_ "Checking if running on an EC2 instance ..."
    !_ <- simpleHttp "http://instance-data/"

    say "Ensuring {} exists ..." [d]
    FS.createTree d

    say "Changing working directory to {} ..." [d]
    FS.setWorkingDirectory d

    log_ "Starting metadata sync ..."
    retrieve d "latest/"
  where
    directory = pathOption "dir"
        ( value "./instance-data"
       <> short 'd'
        ) "Path to output the synced metadata."

    retrieve d p = do
        say "Retrieving {} ..." [p]
        !lbs <- simpleHttp ("http://169.254.169.254/" ++ Text.unpack p)

        say "Writing {} ..." [p]
        FS.withFile (d </> Path.fromText (suffix p)) WriteMode (`LBS.hPut` lbs)

        when ("/" `Text.isSuffixOf` p) $
            mapM_ (retrieve d . mappend p)
                  (Text.lines . LText.toStrict $ LText.decodeUtf8 lbs)

    suffix x = fromMaybe x (Text.stripSuffix "/" x)
