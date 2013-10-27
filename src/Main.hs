-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import qualified Khan.CLI.Ephemeral    as Ephemeral
import qualified Khan.CLI.Persistent as Persistent

-- import qualified Khan.CLI.DNS         as DNS
-- import qualified Khan.CLI.Group       as Group
-- import qualified Khan.CLI.Host        as Host
-- import qualified Khan.CLI.Metadata    as Metadata
-- import qualified Khan.CLI.Profile     as Profile
import           Khan.Internal

main :: IO ()
main = runProgram
     $ Ephemeral.commands
    ++ Persistent.commands

    -- , group "Persistent:"
    --     [ 
    --     ]

    -- , group "Direct:"
    --     [ DNS.cli
    --     , Group.cli
    --     , Metadata.cli
    --     , Profile.cli
    --     ]
    -- ]
