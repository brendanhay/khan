{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.CLI.Chef
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Chef (cli) where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding     as Text
import qualified Khan.AWS.EC2           as EC2
import qualified Khan.AWS.IAM           as IAM
import           Khan.Internal
import           Network.AWS
import           System.Random          (randomRIO)

defineOptions "Launch" $ do
    textOption "lRole" "role" ""
        "Instance's Chef role."

    textOption "lEnv" "env" defaultEnv
        "Instance's environment."

    textOption "lDomain" "domain" ""
        "Instance's DNS domain."

    maybeTextOption "lImage" "image" ""
        "Id of the image/ami."

    integerOption "lMin" "min" 1
        "Minimum number of instances to launch."

    integerOption "lMax" "max" 1
        "Maximum number of instances to launch."

    textsOption "lGroups" "groups" []
        "Security groups."

    stringOption "lData" "user-data" "./config/user-data"
        "Path to user data file."

    instanceTypeOption "lType" "type" M1_Small
        "Instance's type."

    boolOption "lOptimised" "optimised" False
        "EBS optimisation."

    rulesOption "lRules" "rules"
        "IP permission specifications."

    stringOption "lZones" "zones" "abc"
         "Availability zones suffixes to provision into (psuedo-random)."

    -- Block Device Mappings
    -- Monitoring
    -- Disable Api Termination
    -- Instance Shutdown Behavior
    -- Client Token
    -- Network Interfaces

deriving instance Show Launch

instance Discover Launch

instance Validate Launch where
    validate Launch{..} = do
        check lRole   "--role must be specified."
        check lEnv    "--env must be specified."
        check lDomain "--domain must be specified."
        check lMin    "--min must be greater than 0."
        check lMax    "--max must be greater than 0."
        check lData   "--user-data must be specified."
        check lZones  "--zones must be specified."

        check (not $ lMin <= lMax)    "--min must be less than or equal to --max."
        checkPath lData $ lData ++    " specified by --user-data must exist."
        check (Within lZones "abcde") "--zones must be within [a-e]."

instance Naming Launch where
    names Launch{..} = unversioned lRole lEnv

defineOptions "Target" $
    textOption "tName" "name" ""
        "Name of the group."

deriving instance Show Target

instance Discover Target
instance Validate Target

cli :: Command
cli = Command "chef" "Manage Chef EC2 Instances."
    [ subCommand "launch" launch
    , subCommand "run"    run
    , subCommand "stop"   stop
    ]

launch :: Launch -> AWS ()
launch l@Launch{..} = do
    ami <- maybe (EC2.findImage [roleName, "base"]) return lImage

    i <- async $ IAM.findRole l
    k <- async $ EC2.createKey l
    s <- async $ EC2.updateGroup (sshGroup lEnv) sshRules
    g <- async $ EC2.updateGroup l lRules

    wait_ i <* logInfo "Found IAM Profile {}" [profileName]
    wait_ k <* logInfo "Found KeyPair {}" [keyName]
    wait_ s <* logInfo "Found SSH Group {}" [sshGroup lEnv]
    wait_ g <* logInfo "Found Role Group {}" [groupName]

    ud  <- liftIO $ Text.decodeUtf8 . Base64.encode <$> BS.readFile lData
    az  <- shuffle lZones
    reg <- currentRegion

    ids <- EC2.runInstances l ami lType (AZ reg az) lMin lMax ud lOptimised
    rs  <- EC2.waitForInstances ids

    EC2.tagInstances l lDomain ids

    liftIO $ print rs

    -- SSH into machine, wget Khan
    -- run setup
  where
    Names{..} = names l

run :: Target -> AWS ()
run Target{..} = return ()

stop :: Target -> AWS ()
stop Target{..} = return ()

shuffle :: MonadIO m => [a] -> m a
shuffle xs = liftIO $ randomRIO (0, length xs - 1) >>= return . (xs !!)
