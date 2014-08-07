{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Khan.Sync.Enumerate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Sync.Enumerate where

import Khan.Prelude
import Khan.Sync.TH
import Network.AWS.EC2.Metadata

class Enumerate a where
    enumerate :: [a]

instance Enumerate Dynamic where
    enumerate = $(nullary ''Dynamic)

instance Enumerate Meta where
    enumerate = $(nullary ''Meta)
      ++ map BlockDevice enumerate
      ++ map IAM enumerate
      ++ map (Network "0") enumerate

instance Enumerate Mapping where
    enumerate = $(nullary ''Mapping)
      ++ [EBS 0, Ephemeral 0]

instance Enumerate Info where
    enumerate = $(nullary ''Info)

instance Enumerate Interface where
    enumerate = $(nullary ''Interface)
