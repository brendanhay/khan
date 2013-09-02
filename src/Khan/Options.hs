{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.Options
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Options where

import           Control.Error
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Language.Haskell.TH
import           Options
import           Options.OptionTypes

defineOptions "KhanOpts" $ do
    boolOption "optDebug" "debug" False
        "Log debug output"

command :: Options a
        => String
        -> (a -> EitherT String IO b)
        -> Subcommand KhanOpts (IO b)
command name action = subcommand name $
    \(_ :: KhanOpts) opts _ -> runScript $ action opts

data RecordType = A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT
    deriving (Eq, Read, Show)

recordTypeOption :: String -> String -> RecordType -> String -> OptionsM ()
recordTypeOption = readOption (ConT ''RecordType)

data Failover = PRIMARY | SECONDARY
    deriving (Eq, Read, Show)

failoverOption :: String -> String -> Failover -> String -> OptionsM ()
failoverOption = readOption (ConT ''Failover)

maybeTextOption :: String -> String -> Text -> String -> OptionsM ()
maybeTextOption name flag def desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = Text.unpack def
        , optionType        = optionTypeMaybe optionTypeText
        , optionDescription = desc
        }

readOption :: (Show a, Read a)
           => Type
           -> String
           -> String
           -> a
           -> String
           -> OptionsM ()
readOption typ name flag def desc =
    customOption name flag def (OptionType typ False readEither [| readEither |]) desc

readEither :: Read a => String -> Either String a
readEither s = note ("Unable to read: " ++ s) $ readMay s

customOption :: Show a
             => String
             -> String
             -> a
             -> OptionType a
             -> String
             -> OptionsM ()
customOption name flag def typ desc =
    option name $ \o -> o
        { optionLongFlags   = [flag]
        , optionDefault     = show def
        , optionType        = typ
        , optionDescription = desc
        }
