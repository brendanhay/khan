{-# LANGUAGE NoImplicitPrelude   #-}{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Internal.Options
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Options
    (
    -- * Default Options
      textOption
    , maybeTextOption
    , textsOption
    , stringOption
    , intOption
    , integerOption
    , maybeIntegerOption
    , boolOption
    , pathOption
    , maybePathOption

    -- * Option Types
    , optionTypeWord8
    , optionTypeList

    -- * Custom Options
    , recordTypeOption
    , failoverOption
    , regionOption
    , routingPolicyOption
    , instanceTypeOption
    , rulesOption
    , versionOption
    , customOption
    ) where

import qualified Data.Text                  as Text
import           Data.Version
import           Khan.Internal.Types
import           Khan.Prelude
import           Language.Haskell.TH
import           Network.AWS.EC2
import           Network.AWS.Internal.Types
import           Network.AWS.Route53
import qualified Options                    as Opts
import           Options.OptionTypes

import Options hiding
     ( textOption
     , textsOption
     , intOption
     , integerOption
     , boolOption
     , stringOption
     , pathOption
     )

type Opt a = String -> String -> a -> String -> OptionsM ()

textOption :: Opt Text
textOption name flag def = Opts.textOption name flag def
    . defaultText (Text.unpack def)

maybeTextOption :: Opt Text
maybeTextOption name flag (Text.unpack -> def) desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = def
        , optionType        = optionTypeMaybe optionTypeText
        , optionDescription = defaultText def desc
        }

textsOption :: Opt [Text]
textsOption name flag def = Opts.textsOption name flag def
    . defaultText (Text.unpack $ Text.intercalate ", " def)

stringOption :: Opt String
stringOption name flag def = Opts.stringOption name flag def
    . defaultText def

intOption :: Opt Int
intOption name flag def = Opts.intOption name flag def
    . defaultText (show def)

integerOption :: Opt Integer
integerOption name flag def = Opts.integerOption name flag def
    . defaultText (show def)

maybeIntegerOption :: Opt Integer
maybeIntegerOption name flag (show -> def) desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = def
        , optionType        = optionTypeMaybe optionTypeInteger
        , optionDescription = defaultText def desc
        }

boolOption :: Opt Bool
boolOption name flag def = Opts.boolOption name flag def
    . defaultText (show def)

pathOption :: Opt FilePath
pathOption name flag def = Opts.pathOption name flag def
    . defaultText (show def)

maybePathOption :: Opt Text
maybePathOption name flag (Text.unpack -> def) desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = def
        , optionType        = optionTypeMaybe optionTypeFilePath
        , optionDescription = defaultText def desc
        }

recordTypeOption :: Opt RecordType
recordTypeOption = readOption (ConT ''RecordType)

failoverOption :: Opt Failover
failoverOption = readOption (ConT ''Failover)

regionOption :: Opt Region
regionOption = readOption (ConT ''Region)

routingPolicyOption :: Opt RoutingPolicy
routingPolicyOption = readOption (ConT ''RoutingPolicy)

instanceTypeOption :: Opt InstanceType
instanceTypeOption = readOption (ConT ''InstanceType)

rulesOption :: String -> String -> String -> OptionsM ()
rulesOption name flag desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = ""
        , optionType        = optionTypeList ',' optionTypeRule
        , optionDescription = desc
        }

versionOption :: String -> String -> Version -> String -> OptionsM ()
versionOption name flag (showVersion -> def) desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = def
        , optionType        = optionTypeVersion
        , optionDescription = defaultText def desc
        }

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

optionTypeVersion :: OptionType Version
optionTypeVersion =
    OptionType (ConT ''Version) False parseVersionE [| parseVersionE |]

optionTypeRule :: OptionType IpPermissionType
optionTypeRule =
    OptionType (ConT ''IpPermissionType) False parseRule [| parseRule |]
