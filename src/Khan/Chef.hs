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
import qualified Data.Text              as Text
import           Khan.Internal
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

    maybeTextOption "cData" "user-data" ""
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
    create c@Create{..} = liftIO (print c)
        -- DescribeKeys
        -- If it khan-{region} doesn't exist create'

        -- Launch Instance
--        RunInstances

        -- Wait for SSH for each one

        -- Login one at a time and invoke chef

    run Target{..} = return ()

    delete Target{..} = return ()
