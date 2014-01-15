{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Image
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Image (commands) where

import Control.Concurrent (threadDelay)
import           Data.Aeson
import           Data.Aeson               as Aeson
import qualified Data.Text                as Text
import qualified Data.Text.Lazy           as LText
import qualified Data.Text.Lazy.Encoding  as LText
import           Data.SemVer
import           Khan.Internal
import qualified Khan.Model.Image         as Image
import qualified Khan.Model.Instance      as Instance
import qualified Khan.Model.Key           as Key
import qualified Khan.Model.Profile       as Profile
import qualified Khan.Model.SecurityGroup as Security
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2
import qualified Shelly                   as Shell

data AMI = AMI
    { aRole     :: !Text
    , aVersion  :: Maybe Version
    , aScript   :: !FilePath
    , aImage    :: !Text
    , aType     :: !InstanceType
    , aPreserve :: !Bool
    }

amiParser :: Parser AMI
amiParser = AMI
    <$> roleOption
    <*> optional versionOption
    <*> pathOption "script" (short 's' <> action "file")
        "Script to pass the image-id as $1 to."
    <*> textOption "base" (short 'b')
        "Id of the base image/ami."
    <*> readOption "type" "TYPE" (value M1_Small)
        "Instance's type."
    <*> switchOption "preserve" False
        "Don't terminate the base instance on error."

instance Options AMI where
    validate AMI{..} =
        checkPath aScript " specified by --script must exist."

instance Naming AMI where
    names AMI{..} = v
        { profileName = "ami-builder"
        , groupName   = sshGroup "ami"
        }
      where
        v = maybe (unversioned aRole "ami")
                  (versioned aRole "ami")
                  aVersion

commands :: Mod CommandFields Command
commands = group "image" "Create AMIs." $ mconcat
    [ command "build" build amiParser
        "AMI."
    ]
