{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

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

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.SemVer
import qualified Data.Text                   as Text
import qualified Data.Text.Format            as Format
import qualified Filesystem.Path.CurrentOS   as Path
import           Khan.CLI.Ansible            (Ansible(..), playbook)
import           Khan.Internal
import           Khan.Internal.Ansible
import qualified Khan.Model.AvailabilityZone as AZ
import qualified Khan.Model.Image            as Image
import qualified Khan.Model.Instance         as Instance
import qualified Khan.Model.Key              as Key
import qualified Khan.Model.Profile          as Profile
import qualified Khan.Model.SSH              as SSH
import qualified Khan.Model.SecurityGroup    as Security
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2             hiding (Failed, Image)
import qualified Shelly                      as Shell

data Image = Image
    { iRole     :: !Role
    , iVersion  :: Maybe Version
    , iPlaybook :: !FilePath
    , iImage    :: !Text
    , iType     :: !InstanceType
    , iPreserve :: !Bool
    , iNDevices :: !Integer
    , iArgs     :: [String]
    , iZones    :: !String
    , iUser     :: !Text
    , iTimeout  :: !Int
    }

imageParser :: Parser Image
imageParser = Image
    <$> roleOption
    <*> optional versionOption
    <*> pathOption "playbook" (short 'p' <> value "")
        "Path to the playbook to run on the base instance."
    <*> textOption "base" (short 'b')
        "Id of the base image/ami."
    <*> readOption "type" "TYPE" (value M1_Small)
        "Instance's type."
    <*> switchOption "preserve" False
        "Don't terminate the base instance on error."
    <*> integralOption "block-devices" (value 8)
        "Number of ephemeral devices to register the ami with."
    <*> argsOption str (action "file")
        "Pass through arugments to ansible."
    <*> stringOption "zones" (value "")
        "Availability zones suffixes to provision into (psuedo-random)."
    <*> userOption
    <*> integralOption "timeout" (value 180)
        "Timeout for SSH connectivity for the launched base instance."

instance Options Image where
    discover _ _ a@Image{..} = return $! a
        { iPlaybook = defaultPath iPlaybook $ Path.fromText "image.yml"
        }

    validate Image{..} =
        check (iNDevices > 24) "--block-devices should be less than 24"

instance Naming Image where
    names Image{..} = ver
        { profileName = "ami-builder"
        }
      where
        ver = maybe (unversioned iRole "ami")
                    (versioned   iRole "ami")
                    iVersion

commands :: Mod CommandFields Command
commands = command "image" image imageParser
    "Create Image."

image :: Common -> Image -> AWS ()
image c@Common{..} d@Image{..} = do
    shell (Shell.which "ansible-playbook") >>=
        void . noteAWS "Command {} doesn't exist." ["ansible-playbook" :: Text]

    log "Checking if target Image {} exists..." [imageName]
    as <- Image.findAll [] [Filter "name" [imageName]]

    unless (null as) $
        throwAWS "Image {} already exists, exiting..." [imageName]

    log "Looking for base Images matching {}" [iImage]
    a <- async $ Image.find [iImage] []

    log "Looking for IAM Profile matching {}" [profileName]
    i <- async $ Profile.find d

    ami <- diritImageId <$> wait a
    log "Found base Image {}" [ami]

    wait_ i <* log "Found IAM Profile {}" [profileName]

    g <- async $ Security.sshGroup d
    z <- async $ AZ.getSuffixes iZones
    k <- async $ Key.create cBucket d cCerts

    wait_ g <* log "Found Role Group {}" [sshGroupName]

    az  <- fmap (AZ cRegion) $ wait z >>= randomSelect
    key <- wait k <* log "Found KeyPair {}" [keyName]

    log "Using AvailabilityZone {}" [Format.Shown az]
    mr  <- listToMaybe . map riitInstanceId <$>
        Instance.run d ami iType az 1 1 False

    iid <- noteAWS "Failed to launch any Instances using Image {}" [ami] mr
    log "Launched Instance {}" [iid]

    Instance.wait [iid]

    e <- contextAWS c $ do
        log "Finding public DNS for {}" [iid]
        mdns <- join . listToMaybe . map riitDnsName <$>
            Instance.findAll [iid] []
        dns  <- noteAWS "Failed to retrieve DNS for {}" [iid] mdns

        unlessM (SSH.wait iTimeout dns iUser key) $
            throwAWS "Failed to gain SSH connectivity for {}" [iid]

        log "Running Playbook {}" [iPlaybook]
        playbook c $ Ansible "ami" Nothing Nothing 36000 False $ iArgs ++
            [ "-i", Text.unpack $ dns <> ",localhost,"
            , "-e", LBS.unpack . Aeson.encode $ ImageInput n cRegion dns
            , Path.encodeString iPlaybook
            ]

        void $ Image.create iid imageName (blockDevices iNDevices)

    if isLeft e && iPreserve
        then log "Error creating Image, preserving base Instance {}" [iid]
        else do
            log_ "Terminating base instance"
            send_ $ TerminateInstances [iid]

    const () <$> hoistError e
  where
    n@Names{..} = names d

    blockDevices i = zipWith mkBlockDevice ['b' .. 'z'] [0 .. i - 1]

    mkBlockDevice dev virt = BlockDeviceMappingItemType
        { bdmitDeviceName  = "/dev/sd" `Text.snoc` dev
        , bdmitVirtualName = Just $ Text.pack $ "ephemeral" <> show virt
        , bdmitEbs         = Nothing
        , bdmitNoDevice    = Nothing
        }
