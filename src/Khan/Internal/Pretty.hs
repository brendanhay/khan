-- Module      : Khan.Internal.Pretty
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Pretty
    ( Pretty (..)
    , prettyPrint
    , layout
    ) where

import Control.Monad.IO.Class
import Data.List              (transpose)
import Text.PrettyPrint.Boxes

class Pretty a where
    pretty :: a -> Box

prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = liftIO . printBox . pretty

layout :: [[String]] -> Box
layout = moveRight 1
    . hsep 2 left
    . map (vcat left . map text)
    . transpose
