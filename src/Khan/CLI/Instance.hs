{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.CLI.Instance
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Instance (cli) where

import Khan.Internal

defineOptions "Group" $
    textOption "gName" "name" ""
        "Name of the group."

deriving instance Show Group

instance Discover Group
instance Validate Group

cli :: Command
cli = Command "instance" "Manage EC2 Instances."
    [ subCommand "describe" describe
    , subCommand "modify"   modify
    , subCommand "delete"   delete
    ]
  where
    describe Group{..} = return ()

    modify Group{..} = return ()

    delete Group{..} = return ()