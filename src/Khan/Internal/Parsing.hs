{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Khan.Internal.Parsing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Parsing where

import           Data.Attoparsec.Text
import qualified Data.Text            as Text
import           Data.Tuple
import           Khan.Prelude
import           Network.AWS.EC2

parseRule :: String -> Either String IpPermissionType
parseRule s = msg . parseOnly parser $ Text.pack s
  where
    msg = fmapL . const $
        "expected: tcp|udp|icmp:from_port:to_port:[group|0.0.0.0,...], got: " ++ s

    parser = do
        p <- protocol
        f <- decimal <* char ':'
        t <- decimal <* char ':'
        g <- sepBy1 (eitherP range group) (char ',')
        return . uncurry (IpPermissionType p f t) . swap $ partitionEithers g

    range = do
        a <- takeTill (== '.') <* char '.'
        b <- takeTill (== '.') <* char '.'
        c <- takeTill (== '.') <* char '.'
        d <- text
        return . IpRange $ Text.intercalate "." [a, b, c, d]

    group = UserIdGroupPair Nothing Nothing <$> (Just <$> text)

    text = Text.pack <$> many1 (satisfy $ notInClass ":|,")

    protocol = do
        p <- takeTill (== ':') <* char ':'
        case p of
            "tcp"  -> return TCP
            "udp"  -> return UDP
            "icmp" -> return ICMP
            _      -> fail ""
