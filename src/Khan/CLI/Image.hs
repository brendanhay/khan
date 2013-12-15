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

import           Khan.Internal
import qualified Khan.Model.Image         as AMI
import qualified Khan.Model.Instance      as Instance
import qualified Khan.Model.Key           as Key
import qualified Khan.Model.Profile       as Profile
import qualified Khan.Model.SecurityGroup as Security
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2

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
    names AMI{..} = unversioned "builder" "ami"

commands :: Mod CommandFields Command
commands = group "image" "Create AMIs." $ mconcat
    [ command "build" build amiParser
        "AMI."
    ]

build :: Common -> AMI -> AWS ()
build Common{..} d@AMI{..} = do
    log "Looking for Images matching {}" [aImage]
    a <- async . AMI.find . (:[]) $ Filter "image-id" [aImage]

    log "Looking for IAM Profiles matching {}" [profileName]
    i <- async $ Profile.find d

    ami <- wait a
    log "Found Image {}" [ami]

    wait_ i <* log "Found IAM Profile {}" [profileName]

    k <- async $ Key.create cBucket d cCerts
    s <- async $ Security.update (sshGroup envName) sshRules

    wait_ k <* log "Found KeyPair {}" [keyName]
    wait_ s <* log "Found SSH Group {}" [sshGroup envName]

    az <- shuffle "bc"
    r  <- Instance.run d ami aType (AZ cRegion az) 1 1 False

    let ids = map riitInstanceId r

    Instance.wait ids

    log "Running {}" [aScript]

    send_ $ TerminateInstances ids
  where
    Names{..} = names d
