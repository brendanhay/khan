{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.Host
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Host
    (
    -- * Host info
      Info (..)
    , ordinal
    , address

    -- * Operations
    , choose
    , findAll
    ) where

import           Khan.Internal
import qualified Khan.Model.EC2.Instance      as Instance
import qualified Khan.Model.Tag               as Tag
import           Khan.Prelude
import           Network.AWS.EC2
import           System.IO                    hiding (FilePath)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), group)

data Info = Info !Int !(Text, Text) !RunningInstancesItemType

instance Pretty Info where
    pretty (Info n (v, a) RunningInstancesItemType{..}) = " " <> int n <> ")"
        <+> addr
        <+> pretty riitInstanceId
        <+> pretty riitImageId
        <+> pretty (istName riitInstanceState)
      where
        addr | v /= a    = wrap a <+> pretty (stripText ".amazonaws.com" v)
             | otherwise = wrap v

        wrap x = "[" <> pretty x <> "]"

ordinal :: Info -> Int
ordinal (Info n _ _) = n

address :: Info -> Text
address (Info _ a _) = snd a

choose :: Bool -> Env -> Role -> (Info -> AWS ()) -> AWS ()
choose vpn env role f = findAll vpn env role >>= go
  where
    go []  = log_ "No hosts found."
    go [x] = f x
    go xs  = do
        mapM_ (pPrint . pretty) xs
        c <- ask
        x <- noteAWS "Invalid host selection '{}'." [c] $
            find ((c ==) . show . ordinal) xs
        f x

    ask = liftIO $ do
        hSetBuffering stdout NoBuffering
        putStr "Select the host to connect to: "
        getLine

findAll :: Bool -> Env -> Role -> AWS [Info]
findAll vpn env role = mapMaybe f . zip [1..] <$> Instance.findAll []
    [ Tag.filter Tag.env  [_env env]
    , Tag.filter Tag.role [_role role]
    ]
  where
    f (n, x) = Info n
        <$> Instance.address vpn x
        <*> pure x
