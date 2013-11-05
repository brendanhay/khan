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
import           Data.Char                       (isDigit)
import qualified Data.Text                       as Text
import           Data.Version
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS.EC2
import qualified Text.ParserCombinators.ReadP    as ReadP

showDNS :: DNS -> Text -> Text
showDNS DNS{..} dom = Text.concat
    [ dnsRole
    , maybe "" (`Text.snoc` '-') $ safeVersion <$> dnsVer
    , Text.pack $ show dnsOrd
    , "."
    , dnsEnv
    , "."
    , dnsReg
    , "."
    , dom
    ]

parseDNS :: Text -> Either String DNS
parseDNS = parseOnly parser
  where
    parser = DNS
        <$> takeTill isDigit
        <*> option Nothing (Just <$> parseSafeVersion <* char '-')
        <*> (decimal <* char '.')
        <*> (takeTill (== '.') <* char '.')
        <*> (takeTill (== '.') <* char '.')

parseSafeVersionM :: Text -> Maybe Version
parseSafeVersionM = hush . parseOnly parseSafeVersion

parseSafeVersion :: Parser Version
parseSafeVersion = do
    ma <- decimal <* char 'm'
    mi <- decimal <* char 'p'
    p  <- decimal
    return $! Version [ma, mi, p] []

eitherVersion :: String -> Either String Version
eitherVersion s = maybe (Left $ "Failed to parse version: " ++ s) (Right . fst)
    . listToMaybe
    . reverse
    . ReadP.readP_to_S parseVersion
    $ map f s
  where
    f '+' = '-'
    f  c  = c

showRules :: Foldable f => f IpPermissionType -> Text
showRules = Text.intercalate ", " . map rule . toList
  where
    rule IpPermissionType{..} = Text.intercalate ":"
        [ Text.pack $ show iptIpProtocol
        , Text.pack $ show iptFromPort
        , Text.pack $ show iptToPort
        , fromMaybe "" groupOrRange
        ]
      where
        groupOrRange = headMay (mapMaybe uigGroupName $ toList iptGroups)
            <|> headMay (map irCidrIp $ toList iptIpRanges)

parseRule :: String -> Either String IpPermissionType
parseRule s = msg . parseOnly parser $ Text.pack s
  where
    msg = fmapL . const $
        "expected: tcp|udp|icmp:from_port:to_port:group|0.0.0.0, got: " ++ s

    parser = do
        p <- protocol
        f <- decimal <* char ':'
        t <- decimal <* char ':'
        g <- eitherP range group
        let perm = IpPermissionType p f t
        return $! either (\x -> perm [] [x]) (\x -> perm [x] []) g

    range = do
        a <- takeTill (== '.') <* char '.'
        b <- takeTill (== '.') <* char '.'
        c <- takeTill (== '.') <* char '.'
        d <- Text.pack <$> many1 anyChar
        return . IpRange $ Text.intercalate "." [a, b, c, d]

    group = UserIdGroupPair Nothing Nothing <$> (Just <$> takeText)

    protocol = do
        p <- takeTill (== ':') <* char ':'
        case p of
            "tcp"  -> return TCP
            "udp"  -> return UDP
            "icmp" -> return ICMP
            _      -> fail ""
