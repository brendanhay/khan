{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.CLI.Group
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Group (cli) where

import qualified Khan.AWS.EC2     as EC2
import           Khan.Internal
import           Network.AWS
import           Pipes
import           Text.Show.Pretty

defineOptions "Group" $ do
    textOption "gName" "name" ""
        "A name."

    textOption "gEnv" "env" defaultEnv
        "Environment of the group."

    rulesOption "gRules" "rules"
        "IP permission specifications."

deriving instance Show Group

instance Discover Group

instance Validate Group where
    validate Group{..} =
        check gName  "--name must be specified."

instance Naming Group where
    names Group{..} = unversioned gName gEnv

cli :: Command
cli = Command "group" "Manage security groups and rules."
    [ subCommand "show"   info
    , subCommand "update" update
    , subCommand "delete" delete
    ]

info :: Group -> AWS ()
info g = EC2.findGroup g >>= liftIO . putStrLn . ppShow

update :: Group -> AWS ()
update g = EC2.updateGroup g (gRules g)

delete :: Group -> AWS ()
delete = EC2.deleteGroup
