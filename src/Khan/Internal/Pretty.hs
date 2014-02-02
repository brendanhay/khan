{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

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
    ( PP (..)
    , prettyPrint
    ) where

import qualified Data.Text.Lazy.IO            as LText
import           Khan.Prelude                 hiding ((<$>), print)
import           Network.AWS
import           Network.AWS.AutoScaling
import           Network.AWS.EC2
import           Text.PrettyPrint.Leijen.Text
import           Text.PrettyPrint.Leijen.Text (Pretty)
import qualified Text.PrettyPrint.Leijen.Text as PP

newtype PP a = PP { pp :: a }

instance Pretty (PP AutoScalingGroup)
instance Pretty (PP RunningInstancesItemType)

prettyPrint :: (MonadIO m, Pretty a) => a -> m ()
prettyPrint = liftIO
    . LText.putStrLn
    . PP.displayT
    . PP.renderPretty 0 80
    . PP.pretty
