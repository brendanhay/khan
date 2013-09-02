{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Error
import Control.Monad
import Data.List
import Khan.Command
import System.Environment
import System.Console.CmdTheLine

main :: IO ()
main = runScript $ do
    (Command{..}, args) <- scriptIO getArgs >>= hoistEither . check
    scriptIO . join $ evalChoice args command subcommands
  where
    check []     = help "No command specified."
    check (a:as) = maybe (help "Invalid command specified.") (Right . (,as))
        $ find ((a ==) . termName . snd . command) cmds

    help msg = Left $ intercalate "\n"
        [ "ERROR:    " ++ msg
        , "SYNOPSIS: khan COMMAND ..."
        , "COMMANDS: " ++ intercalate ", " names
        ]

    names = map (termName . snd . command) cmds
    cmds  = [dns]
