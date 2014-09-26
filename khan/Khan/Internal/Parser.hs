{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Internal.Parser
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Parser
    (
    -- * Version
      fileNameVersion

    -- * Text
    , TextParser (..)
    , parseString
    , parseDelimited
    , parseText
    ) where

import           Control.Monad
import           Data.Attoparsec.Text
import qualified Data.SemVer          as Ver
import           Data.SemVer          (Version)
import qualified Data.Text            as Text
import           Data.Tuple
import           Khan.Prelude         hiding (find, min, max)
import qualified Network.AWS.EC2      as EC2
import           Network.AWS.EC2      hiding (Protocol(..), Instance)

fileNameVersion :: Text -> Maybe Version
fileNameVersion = lastMay . Text.split (== '/') >=> hush . Ver.fromText

parseString :: TextParser a => String -> Either String a
parseString = parseText . Text.pack

parseDelimited :: TextParser a => Char -> String -> Either String [a]
parseDelimited c = traverse parseText . Text.split (== c) . Text.pack

parseText :: TextParser a => Text -> Either String a
parseText = parseOnly parser

class TextParser a where
    parser :: Parser a

instance TextParser IpPermissionType where
    parser = do
        p <- parser
        f <- decimal <* char ':'
        t <- decimal <* char ':'
        g <- sepBy1 (eitherP parser parser) (char ',')
        return . uncurry (IpPermissionType p f t)
               . swap
               $ partitionEithers g

instance TextParser IpRange where
    parser = do
        a <- takeTill (== '.') <* char '.'
        b <- takeTill (== '.') <* char '.'
        c <- takeTill (== '.') <* char '.'
        d <- segment
        return . IpRange $ Text.intercalate "." [a, b, c, d]

instance TextParser UserIdGroupPair where
    parser = UserIdGroupPair Nothing Nothing <$> (Just <$> segment)

instance TextParser EC2.Protocol where
    parser = do
        p <- takeTill (== ':') <* char ':'
        case p of
            "tcp"  -> return EC2.TCP
            "udp"  -> return EC2.UDP
            "icmp" -> return EC2.ICMP
            _      -> return EC2.TCP

segment :: Parser Text
segment = Text.pack <$> many1 (satisfy $ notInClass ":|,")
