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

import           Control.Monad              (mplus)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Format           as Format
import qualified Data.Text.Lazy.IO          as LText
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.Instance        as Instance
import qualified Khan.Model.Key             as Key
import           Khan.Prelude
import           Network.AWS.EC2            hiding (Failed)
import           System.Directory
import qualified System.Posix.Files         as Posix
import qualified System.Posix.Process       as Posix

data AMI = AMI
    { aScript   :: !FilePath
    , aImage    :: !Text
    , aType     :: !InstanceType
    , aPreserve :: !Bool
    }

amiParser :: Parser AMI
amiParser = AMI
    <$> pathOption "script" (short 's' <> action "file")
        "Script to pass the image-id as $1 to."
    <*> textOption "image" (short 'i')
        "Id of the base image/ami."
    <*> readOption "type" "TYPE" (value M1_Small)
        "Instance's type."
    <*> switchOption "preserve" False
        "Don't terminate the base instance on error."

instance Options AMI where
    validate AMI{..} = do
        checkPath aScript " specified by --script must exist."

instance Naming AMI where
    names AMI{..} = unversioned "base" "ami"

commands :: Mod CommandFields Command
commands = group "image" "Create AMIs." $ mconcat
    [ command "build" build amiParser
        "AMI."
    ]

build :: Common -> AMI -> AWS ()
build c@Common{..} a@AMI{..} = return ()


-- Takes a bash script, region, az etc
-- Does the keypair/secgroup dance
-- Launch instance
-- Runs the bash script with $1 == instance id
-- Create image
-- Wait for imate
-- Tag image
-- Terminate instance
