{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Chef
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Chef (command) where

import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Khan.Groups
import           Khan.Internal
import           Khan.Keys
import           Network.AWS
import           Network.AWS.EC2

defineOptions "Create" $ do
    textOption "cRole" "role" ""
        "Instance's Chef role."

    textOption "cEnv" "env" defaultEnv
        "Instance's environment."

    textOption "cDomain" "domain" ""
        "Instance's DNS domain."

    textOption "cImage" "image" ""
        "Id of the image/ami."

    integerOption "cMin" "min" 1
        "Minimum number of instances to launch."

    integerOption "cMax" "max" 1
        "Maximum number of instances to launch."

    textsOption "cGroups" "groups" []
        "Security groups."

    stringOption "cData" "user-data" "./config/user-data"
        "Path to user data file."

    instanceTypeOption "cType" "type" M1_Small
        "Instance's type."

    boolOption "cOptimised" "optimised" False
        "EBS optimisation."

    -- Block Device Mappings
    -- Monitoring
    -- Disable Api Termination
    -- Instance Shutdown Behavior
    -- Client Token
    -- Network Interfaces

deriving instance Show Create

instance Discover Create where
    discover c@Create{..}
        | invalid cRole        = return c
        | not $ invalid cImage = return c
        | otherwise            = findImage
       where
         findImage = do
            logInfo "Looking for AMIs matching: {}" [options]
            ami <- (listToMaybe . djImagesSet) <$>
                send (DescribeImages [] [] ["self"] filters) >>=
                noteError "Failed to find any AMIs"
            return $! c { cImage = diritImageId ami }

         options = Text.intercalate " | " images
         filters = [Filter "tag:Name" images]
         images  = [cRole, "base"]

instance Validate Create where
    validate Create{..} = do
        check cRole   "--role must be specified."
        check cEnv    "--env must be specified."
        check cDomain "--domain must be specified."
        check cImage  "--image must be specified."
        check cMin    "--min must be greater than 0."
        check cMax    "--max must be greater than 0."
        check (not $ cMin <= cMax) "--min must be less than or equal to --max."
        checkPath cData $ cData ++ " specified by --user-data must exist."

defineOptions "Target" $
    textOption "tName" "name" ""
        "Name of the group."

deriving instance Show Target

instance Discover Target
instance Validate Target

command :: Command
command = Command "chef" "Manage Chef EC2 Instances."
    [ subCommand "create" create
    , subCommand "run"    run
    , subCommand "delete" delete
    ]
  where
    create Create{..} = do
        createGroup sshGroup (Just sshPort)
        createGroup role Nothing
        createKey key

        u <- liftIO $ Text.readFile cData

        r <- send $ RunInstances
            cImage
            cMin
            cMax
            (Just key)
            []               -- Group Ids
            [sshGroup, role] -- Group Names
            (Just u)         -- User Data
            (Just cType)
            Nothing
            Nothing
            Nothing
            []               -- Block Devices
            (Just $ MonitoringInstanceType True)
            Nothing
            Nothing          -- FIXME: Disable API Termination
            (Just "stop")    -- Shutdown Behaviour
            Nothing          -- Private IP
            Nothing          -- Client Token
            []               -- NICs
            [IamInstanceProfileRequestType Nothing (Just role)]
            (Just cOptimised)

        liftIO $ print r

        -- Launch Instance
--        RunInstances

        -- Wait for SSH for each one

        -- Login one at a time and invoke chef
      where
        role = cRole <> "-" <> cEnv
        key  = cEnv

    run Target{..} = return ()

    delete Target{..} = return ()
