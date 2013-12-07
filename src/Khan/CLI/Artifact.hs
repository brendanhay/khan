{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.HealthCheck    as Check
import qualified Khan.Model.Object         as Obj
import           Khan.Prelude              hiding (for)
import           Network.AWS.Route53

data Artifact = Artifact
    { aBucket :: !Text
    , aKey    :: !Text
    , aPath   :: !FilePath
    , aForce  :: !Bool
    } deriving (Show)

artifactParser :: Parser Artifact
artifactParser = Artifact
    <$> textOption "bucket" (short 'b')
        "Bucket."
    <*> textOption "key" (short 'k')
        "Key."
    <*> pathOption "file" (short 'f')
        "Local file."
    <*> switchOption "force" False
        "Overwrite if exists."

instance Options Artifact

commands :: Mod CommandFields Command
commands = mconcat
    [ command "upload" upload artifactParser
        "Upload an artifact to S3."
    , command "download" download artifactParser
        "Download an artifact to disk."
    ]

upload :: Common -> Artifact -> AWS ()
upload c@Common{..} Artifact{..} = do
    return ()

download :: Common -> Artifact -> AWS ()
download c@Common{..} Artifact{..} = void $ Obj.download aBucket aKey aPath
