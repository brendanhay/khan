{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.CLI.Group
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Group (commands) where

import qualified Khan.AWS.EC2  as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS

defineOptions "Group" $ do
    textOption "gRole" "role" ""
        "Role of the application."

    textOption "gEnv" "env" defaultEnv
        "Environment of the application."

    rulesOption "gRules" "rules"
        "IP permission specifications."

deriving instance Show Group

instance Discover Group

instance Validate Group where
    validate Group{..} = do
        check gRole "--role must be specified."
        check gEnv  "--env must be specified."

instance Naming Group where
    names Group{..} = unversioned gRole gEnv

commands :: [Command]
commands =
    [ command group "group" "Create or update Security Groups."
        "Comprehensive rule specification, or else!"
    ]

group :: Group -> AWS ()
group g = EC2.updateGroup g (gRules g)

-- info :: Group -> AWS ()
-- info g = EC2.findGroup g >>= liftIO . putStrLn . ppShow

-- delete :: Group -> AWS ()
-- delete = EC2.deleteGroup

