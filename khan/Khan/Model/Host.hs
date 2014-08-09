{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

import           Data.Proxy
import qualified Data.Text                    as Text
import           Khan.Internal
import qualified Khan.Model.EC2.Instance      as Instance
import qualified Khan.Model.Tag               as Tag
import           Khan.Prelude
import           Network.AWS.EC2
import           System.IO                    hiding (FilePath)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>), group)

default (Text)

data Info = Info !Int !(Text, Text) (Ann RunningInstancesItemType)

instance Header Info where
    header _ = hcols 15
        [ W 3  (H "n:")
        , W 18 (H "dns:")
        , W 18 (H "ssh-address:")
        , W 8  (H "state:")
        , W 8  (H "weight:")
        , W 10 (H "version:")
        , H "instance-id:"
        , H "image-id:"
        , H "type:"
        , W 19 (H "launched:")
        ]

instance Body Info where
    body (Info n (v, a) (Ann RunningInstancesItemType{..} Tags{..})) = hcols 15
        [ W 3  (C $ int n <> ")")
        , W 18 (C $ Text.takeWhile (/= '.') v)
        , W 18 (C a)
        , W 8  (C $ istName riitInstanceState)
        , W 8  (C tagWeight)
        , W 10 (C tagVersion)
        , C riitInstanceId
        , C riitImageId
        , C riitInstanceType
        , W 19 (C riitLaunchTime)
        ]

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
        pPrint $ title "ssh"
             <-> header (Proxy :: Proxy Info)
             <-> body xs
             <-> mempty
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
        <*> Tag.annotate x
