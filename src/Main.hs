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

import Khan.Internal

import qualified Khan.CLI.Application as Application
import qualified Khan.CLI.Chef        as Chef
import qualified Khan.CLI.DNS         as DNS
import qualified Khan.CLI.Group       as Group
import qualified Khan.CLI.Instance    as Instance
import qualified Khan.CLI.Metadata    as Metadata
import qualified Khan.CLI.Profile     as Profile

main :: IO ()
main = runProgram
    [ group "Ephemeral:"
        [ Application.cli
        , Instance.cli
        ]

    , group "Persistent:"
        [ Chef.cli
        ]

    , group "Direct:"
        [ DNS.cli
        , Group.cli
        , Metadata.cli
        , Profile.cli
        ]
    ]
