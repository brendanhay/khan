{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Khan.Command.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Command.Internal
    (
    -- * Top-level Command Wrapper
      Command(..)
    , newCommand

    -- * Vinyl Operators
    , (^$)
    , (=^$)
    ) where

import Control.Applicative
import Data.Vinyl
import GHC.TypeLits
import System.Console.CmdTheLine

data Command = Command
    { command     :: (Term (IO ()), TermInfo)
    , subcommands :: [(Term (IO ()), TermInfo)]
    }

newCommand :: String -> [(Term (IO ()), TermInfo)] -> Command
newCommand name = Command
    (ret . pure $ helpFail Plain Nothing, defTI { termName = name })

(^$) :: (IElem (sy ::: a) rs) => PlainRec rs -> (sy ::: a) -> a
record ^$ field = record ^. rLens field

(=^$) :: forall sy f t. (Applicative f, SingI sy) => sy ::: t -> (String -> f t) -> Rec '[sy ::: t] f
a =^$ b = a =^ b (fromSing (sing :: Sing sy))

(=^) :: Applicative f => sy ::: t -> f t -> Rec '[sy ::: t] f
_ =^ b = b :& RNil
