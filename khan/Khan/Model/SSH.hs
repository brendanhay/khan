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
     ( Mode (..)
     , execSCP
     , execSSH
     , wait
     ) where

import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude
import           System.Process            (callCommand)

data Mode
    = Upload   !FilePath !FilePath
    | Download !FilePath !FilePath
      deriving (Show)

execSCP :: MonadIO m => Mode -> Text -> Text -> FilePath -> [String] -> m ()
execSCP mode addr user key xs = exec "scp" (as ++ xs)
  where
    as = map Text.unpack $ ident : case mode of
        Upload   s d -> [toTextIgnore s, remote d]
        Download s d -> [remote s, toTextIgnore d]

    remote = mappend (user <> "@" <> addr <> ":") . toTextIgnore
    ident  = "-i" <> toTextIgnore key

execSSH :: MonadIO m
        => Text
        -> Text
        -> FilePath
        -> [String]
        -> m ()
execSSH addr user key xs = exec "ssh" (args addr user key xs)

wait :: MonadIO m => Int -> Text -> Text -> FilePath -> m Bool
wait s addr user key = do
    say "Waiting {} seconds for SSH connectivity on {}"
        [show s, Text.unpack addr]
    liftIO (go s)
  where
    delay = 20

    go n | n <= 0    = return False
         | otherwise = do
             say "Waiting {} seconds..." [delay]
             delaySeconds delay
             e <- runEitherT . sync $ exec "ssh" xs
             either (const . go $ n - delay)
                    (return . const True)
                    e

    xs = args addr user key
        [ "-q"
        , "-o"
        , "BatchMode=yes"
        , "-o"
        , "StrictHostKeyChecking=no"
        , "exit"
        ]

exec :: MonadIO m => String -> [String] -> m ()
exec run xs =
    let cmd = unwords (run : xs)
    in log "{}" [cmd] >> liftIO (callCommand cmd)

args :: Text -> Text -> FilePath -> [String] -> [String]
args addr user key = mappend
    [ "-i" <> Path.encodeString key
    , Text.unpack $ user <> "@" <> addr
    ]
