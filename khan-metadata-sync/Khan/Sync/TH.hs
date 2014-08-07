{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Khan.Sync.TH
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Sync.TH where

import Khan.Prelude
import Language.Haskell.TH

nullary :: Name -> Q Exp
nullary name = ListE . mapMaybe expr . ctors <$> reify name
  where
    ctors (TyConI (DataD _ _ _ cs _)) = cs
    ctors _                           = []

    expr (NormalC n []) = Just (ConE n)
    expr _              = Nothing
