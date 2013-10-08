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

import           Khan.Internal

import qualified Khan.CLI.Application as Application
import qualified Khan.CLI.Instance    as Instance
import qualified Khan.CLI.Chef        as Chef

-- import qualified Khan.CLI.Artifact    as Artifact
-- import qualified Khan.CLI.Metadata    as Metadata

import qualified Khan.CLI.DNS         as DNS
import qualified Khan.CLI.Role        as Role
import qualified Khan.CLI.Group    as Group

main :: IO ()
main = runProgram
    [ Application.cli
    , Instance.cli
    ]

    [ Chef.cli
    , DNS.cli
    , Role.cli
    , Group.cli
    ]

    -- -- Low Level
    -- [ Artifact.cli
    -- , DNS.cli
    -- , Metadata.cli
    -- , Role.cli
    -- , Security.cli
    -- ]
