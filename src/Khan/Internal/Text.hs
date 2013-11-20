{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Khan.Internal.Text
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Text where

import qualified Data.Text    as Text
import           Khan.Prelude

tstrip :: Text -> Text -> Text
tstrip x y =
    let z = fromMaybe y $ Text.stripPrefix x y
    in  fromMaybe z $ Text.stripSuffix x z

tappend :: Text -> Text -> Text
tappend x y
    | y `Text.isSuffixOf` x = x
    | otherwise             = x <> y
