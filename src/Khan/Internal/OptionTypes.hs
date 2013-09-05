{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Internal.OptionTypes
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.OptionTypes
    (
    -- * Default Options
      textOption
    , maybeTextOption
    , textsOption
    , integerOption
    , intOption
    , boolOption

    -- * Option Types
    , optionTypeWord8

    -- * Custom Options
    , recordTypeOption
    , failoverOption
    , regionOption
    , routingPolicyOption
    , customOption
    ) where

import           Control.Error
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Khan.Internal.Types
import           Language.Haskell.TH
import           Network.AWS.Internal.Types
import           Network.AWS.Route53
import qualified Options                    as Opts
import           Options                    hiding (textOption, textsOption, integerOption, boolOption)
import           Options.OptionTypes

type Opt a = String -> String -> a -> String -> OptionsM ()

textOption :: Opt Text
textOption name flag def = Opts.textOption name flag def . defaultText (Text.unpack def)

maybeTextOption :: Opt Text
maybeTextOption name flag (Text.unpack -> def) desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = def
        , optionType        = optionTypeMaybe optionTypeText
        , optionDescription = defaultText def desc
        }

textsOption :: Opt [Text]
textsOption name flag def = Opts.textsOption name flag def . defaultText (Text.unpack $ Text.intercalate ", " def)

integerOption :: Opt Integer
integerOption name flag def = Opts.integerOption name flag def . defaultText (show def)

boolOption :: Opt Bool
boolOption name flag def = Opts.boolOption name flag def . defaultText (show def)

recordTypeOption :: Opt RecordType
recordTypeOption = readOption (ConT ''RecordType)

failoverOption :: Opt Failover
failoverOption = readOption (ConT ''Failover)

regionOption :: Opt Region
regionOption = readOption (ConT ''Region)

routingPolicyOption :: Opt RoutingPolicy
routingPolicyOption = readOption (ConT ''RoutingPolicy)

customOption :: Show a
             => String
             -> String
             -> a
             -> OptionType a
             -> String
             -> OptionsM ()
customOption name flag (show -> def) typ desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = def
        , optionType        = typ
        , optionDescription = defaultText def desc
        }

--
-- Internal
--

readOption :: (Show a, Read a)
           => Type
           -> String
           -> String
           -> a
           -> String
           -> OptionsM ()
readOption typ name flag def =
    customOption name flag def (OptionType typ False readEither [| readEither |])

readEither :: Read a => String -> Either String a
readEither s = note ("Unable to read: " ++ s) $ readMay s

defaultText :: String -> String -> String
defaultText ""  desc = desc
defaultText def desc = desc ++ " default: " ++ def
