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

import qualified Khan.Application as Application

import qualified Khan.Artifact    as Artifact
import qualified Khan.DNS         as DNS
import qualified Khan.Metadata    as Metadata
import qualified Khan.Role        as Role
import qualified Khan.Security    as Security

main :: IO ()
main = runProgram

    -- Workflow
    [ Application.command
    ]

    -- Low Level
    [ Artifact.command
    , DNS.command
    , Metadata.command
    , Role.command
    , Security.command
    ]
