{-# LANGUAGE TemplateHaskell #-}

-- Module      : Khan.TH.Metadata
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.TH.Metadata where

import           Control.Applicative
import           Control.Monad
import           Data.Data
import           Data.Foldable            (foldl')
import           Data.Maybe
import           Data.Proxy
import           Data.Text                (Text)
import           Debug.Trace
import           Khan.Internal
import qualified Khan.Model.Host          as Host
import qualified Khan.Model.Key           as Key
import qualified Khan.Model.SSH           as SSH
import qualified Khan.Model.Tag           as Tag
import           Language.Haskell.TH
import           Network.AWS.EC2.Metadata

-- make a class which returns the list of types
-- use th to generate the functions, but can then append
-- additional paths/types manually

class Enumerate a where
    enumerate :: [a]

instance Enumerate Meta where
    enumerate = $(th ''Meta)

th :: Name -> Q Exp
th name = ListE . mapMaybe con . ctors <$> reify name
  where
    ctors (TyConI (DataD _ _ _ cs _)) = cs
    ctors _                           = []

    con (NormalC n []) = Just (ConE n)
    con _              = Nothing
