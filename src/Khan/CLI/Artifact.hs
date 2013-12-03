{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Khan.CLI.Artifact
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Artifact (commands) where

import qualified Data.Attoparsec.Text      as P
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.HealthCheck    as Check
import           Khan.Prelude              hiding (for)
import           Network.AWS.Route53

data Artifact
    = Remote Text Text
    | Local FilePath
      deriving (Show)

pathsParser :: Parser (Artifact, Artifact)
pathsParser = (,)
    <$> customOption "src" "SOURCE" parser (short 's')
        "Source path."
    <*> customOption "dest" "DEST" parser (short 'd')
        "Destination path."

instance Options (Artifact, Artifact)

commands :: Mod CommandFields Command
commands = command "artifact" copy pathsParser
    "Copy Artifacts."

copy :: Common -> (Artifact, Artifact) -> AWS ()
copy c@Common{..} (src, dest) = liftIO $ do
    print src
    print dest

parser :: String -> Either String Artifact
parser s = flip P.parseOnly text $
    if "s3://" `Text.isPrefixOf` text
        then P.string "s3://" *> (Remote <$> P.takeTill (== '/') <*> rest)
        else Local . Path.fromText <$> rest
  where
    text = Text.pack s
    rest = P.takeWhile1 (const True) <* P.endOfInput
