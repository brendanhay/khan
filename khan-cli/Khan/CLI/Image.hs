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

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.SemVer
import qualified Data.Text                       as Text
import qualified Filesystem.Path.CurrentOS       as Path
import           Khan.CLI.Ansible                (Ansible(..), playbook)
import           Khan.Internal
import           Khan.Model.Ansible
import qualified Khan.Model.EC2.AvailabilityZone as AZ
import qualified Khan.Model.EC2.Image            as Image
import qualified Khan.Model.EC2.Instance         as Instance
import qualified Khan.Model.EC2.SecurityGroup    as Security
import qualified Khan.Model.IAM.Role             as Role
import qualified Khan.Model.Key                  as Key
import qualified Khan.Model.SSH                  as SSH
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2                 hiding (Failed, Image)

data Image = Image
    { iRKeys    :: !RKeysBucket
    , iRole     :: !Role
    , iVersion  :: Maybe Version
    , iPlaybook :: !FilePath
    , iImage    :: !Text
    , iType     :: !InstanceType
    , iPreserve :: !Bool
    , iNDevices :: !Integer
    , iArgs     :: [String]
    , iZones    :: [Char]
    , iUser     :: !Text
    , iTimeout  :: !Int
    }

imageParser :: EnvMap -> Parser Image
imageParser env = Image
    <$> rKeysOption env
    <*> roleOption
    <*> optional versionOption
    <*> pathOption "playbook" (short 'p' <> value "")
        "Playbook to run on the base instance."
    <*> textOption "base" (short 'b')
        "Id of the base image/ami."
    <*> readOption "type" "TYPE" (value M3_Medium)
        "Instance's type."
    <*> switchOption "preserve" False
        "Don't terminate the base instance on error."
    <*> integralOption "block-devices" (value 8)
        "Number of ephemeral devices to register the AMI with."
    <*> argsOption str (action "file")
        "Pass through arugments to ansible."
    <*> stringOption "zones" (value [])
        "Availability zones suffixes to provision into."
    <*> userOption
    <*> integralOption "timeout" (value 180)
        "SSH timeout for the launched base instance."

instance Options Image where
    discover _ _ i@Image{..} = return $! i
        { iPlaybook = defaultPath iPlaybook $ Path.fromText "image.yml"
        }

    validate Image{..} = do
        checkFile iPlaybook    " specified by --playbook must exist."
        check (iNDevices > 24) "--block-devices should be less than 24"

instance Naming Image where
    names Image{..} = ver
        { profileName = "ami-builder"
        }
      where
        ver = maybe (unversioned iRole "ami")
                    (versioned   iRole "ami")
                    iVersion

commands :: EnvMap -> Mod CommandFields Command
commands env = command "image" image (imageParser env)
    "Create an Amazon Virtual Machine Image using EC2 and Ansible."

image :: Common -> Image -> AWS ()
image c@Common{..} d@Image{..} = do
    liftEitherT (which "ansible-playbook")

    say "Checking if target Image {} exists..." [imageName]
    as <- Image.findAll [] [Filter "name" [imageName]]

    unless (null as) $
        throwAWS "Image {} already exists, exiting..." [imageName]

    say "Searching for base Images matching {}" [iImage]
    a <- async $ Image.find [iImage] []

    say "Searching for IAM Profile matching {}" [profileName]
    i <- async $ Role.find d

    ami <- diritImageId <$> wait a
    say "Found base Image {}" [ami]

    wait_ i <* say "Found IAM Profile {}" [profileName]

    z <- async $ AZ.getSuffixes iZones
    k <- async $ Key.create iRKeys d cLKeys
    g <- async $ Security.createDefaults d >> Security.sshAccess d

    az  <- fmap (AZ cRegion) $ wait z >>= randomSelect
    key <- wait k <* say "Found KeyPair {}" [keyName]

    wait_ g

    say "Using AvailabilityZone {}" [az]
    mr  <- listToMaybe . map riitInstanceId <$>
        Instance.run d ami iType az 1 1 False

    iid <- noteAWS "Failed to launch any Instances using Image {}" [ami] mr
    say "Launched Instance {}" [iid]

    Instance.wait [iid]

    e <- contextAWS c $ do
        say "Finding public DNS for {}" [iid]
        maddr    <- join . fmap (Instance.address cVPN) <$> Instance.find iid
        (_, dns) <- noteAWS "Failed to retrieve Address for {}" [iid] maddr

        p <- SSH.wait iTimeout dns iUser key
        unless p $
            throwAWS "Failed to gain SSH connectivity for {}" [iid]

        say "Running Playbook {}" [iPlaybook]
        playbook c $ Ansible "ami" iRKeys Nothing Nothing 36000 False $ iArgs ++
            [ "-i", Text.unpack $ dns <> ",localhost,"
            , "-e", encode $ ImageInput n cRegion dns
            , Path.encodeString iPlaybook
            ]

        void $ Image.create d iid (blockDevices iNDevices)

    if isLeft e && iPreserve
        then say "Error creating Image, preserving base Instance {}" [iid]
        else do
            log "Terminating base Instance {}" [iid]
            send_ $ TerminateInstances [iid]

    const () <$> hoistError e
  where
    n@Names{..} = names d

    encode x = '\'' : LBS.unpack (Aeson.encode x <> "'")

    blockDevices i = zipWith mkBlockDevice ['b' .. 'z'] [0 .. i - 1]

    mkBlockDevice dev virt = BlockDeviceMappingItemType
        { bdmitDeviceName  = "/dev/sd" `Text.snoc` dev
        , bdmitVirtualName = Just $ Text.pack $ "ephemeral" <> show virt
        , bdmitEbs         = Nothing
        , bdmitNoDevice    = Nothing
        }
