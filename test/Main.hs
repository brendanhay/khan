{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- Module      : Main
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Data.SemVer
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Khan.Internal
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.SmallCheck

main :: IO ()
main = defaultMain $ testGroup "Khan"
    [ testGroup "Naming"
        [ testProperty "Env . envName . unversioned r == id" $
            \r e   -> envName (unversioned r e) == _env e

        , testProperty "Env . envName . versioned r == id" $
            \r e v -> envName (versioned r e v) == _env e

        , testProperty "Role . roleName . flip unversioned e == id" $
            \r e   -> roleName (unversioned r e) == _role r

        , testProperty "Role . roleName . flip versioned e == id" $
            \r e v -> roleName (versioned r e v) == _role r
        ]
    ]

instance Monad m => Serial m Role where
    series = newtypeCons Role

instance Monad m => Serial m Env where
    series = newtypeCons Env

instance Monad m => Serial m Version where
    series = decDepth . localDepth (const 1) $ cons3 newVersion

instance Monad m => Serial m Text where
    series = Text.pack . getNonEmpty <$> series
