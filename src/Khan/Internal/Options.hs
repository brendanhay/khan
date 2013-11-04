{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
    ( Command (..)
    , Common  (..)

    , commonParser
    , initialise

    , group
    , command

    , readOption
    , switchOption
    , textOption
    , pathOption
    , stringOption
    , integerOption
    , customOption
    , argsOption

    , roleOption
    , envOption
    , versionOption
    , keyOption

    , check
    , checkIO
    , checkPath

    , module Export
    ) where

import qualified Data.Text                 as Text
import           Data.Version
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal.Defaults
import           Khan.Internal.IO
import           Khan.Internal.Parsing
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import           Options.Applicative       as Export hiding (command, info)
import qualified Options.Applicative       as Options
import qualified Shelly                    as Shell
import           System.Environment

data Command where
    Command :: Options a => (Common -> a -> AWS ()) -> a -> Command

data Common = Common
    { cDebug   :: !Bool
    , cRegion  :: !Region
    , cProfile :: Maybe Text
    , cAccess  :: !String
    , cSecret  :: !String
    } deriving (Show)

commonParser :: Parser Common
commonParser = Common
    <$> switchOption 'D' "debug" False
        "Log debug output."
    <*> readOption 'R' "region" "REGION" (value NorthCalifornia)
        "Region to operate in."
    <*> optional (textOption 'P' "iam-profile" mempty
        "IAM profile to use.")
    <*> stringOption 'A' "access-key" (value "")
        "AWS access key."
    <*> stringOption 'S' "secret-key" (value "")
        "AWS secret key."

instance Options Common where
    validate Common{..} = do
        check (null cAccess && profile) $ msg "--access-key" accessKey
        check (null cSecret && profile) $ msg "--secret-key" secretKey
      where
        profile = isNothing cProfile
        msg k e = concat
            [ k
            , " must be specified or "
            , e
            , " env must be set if --iam-role is not set."
            ]

initialise :: (Applicative m, MonadIO m) => Common -> EitherT AWSError m Common
initialise o@Common{..}
    | isJust cProfile = right o
    | validKeys         = right o
    | otherwise         = lookupKeys
  where
    validKeys = (not . null) `all` [cAccess, cSecret]

    lookupKeys = fmapLT toError . syncIO $ do
        acc <- env accessKey cAccess
        sec <- env secretKey cSecret
        return $! o { cAccess = acc, cSecret = sec }
      where
        env k v
            | null v    = fromMaybe "" <$> lookupEnv k
            | otherwise = return v

group ::  String -> String -> Mod CommandFields a -> Mod CommandFields a
group name desc cs = Options.command name $
    Options.info (hsubparser cs) (progDesc desc <> fullDesc)

command :: Options a
        => String
        -> (Common -> a -> AWS ())
        -> Parser a
        -> String
        -> Mod CommandFields Command
command name f p desc = Options.command name $
    Options.info (Command f <$> p) (progDesc desc <> fullDesc)

readOption :: Read a
           => Char
           -> String
           -> String
           -> Mod OptionFields a
           -> String
           -> Parser a
readOption s l typ m desc = option $ mconcat
    [short s, long l, metavar typ, reader auto, help desc, m]

switchOption :: Char -> String -> Bool -> String -> Parser Bool
switchOption s l p desc = flag p (not p) $ short s <> long l <> help desc

textOption :: Char -> String -> Mod OptionFields Text -> String -> Parser Text
textOption s l = customOption s l "STR" (Right . Text.pack)

pathOption :: Char
           -> String
           -> Mod OptionFields FilePath
           -> String
           -> Parser FilePath
pathOption s l = customOption s l "PATH" (Right . Path.decodeString)

stringOption :: Char
             -> String
             -> Mod OptionFields String
             -> String
             -> Parser String
stringOption s l m desc = strOption $ mconcat
    [ short s
    , long l
    , metavar "STR"
    , help desc
    , m
    ]

integerOption :: Char
              -> String
              -> Mod OptionFields Integer
              -> String
              -> Parser Integer
integerOption s l = readOption s l "INT"

customOption :: Char
             -> String
             -> String
             -> (String -> Either String a)
             -> Mod OptionFields a
             -> String
             -> Parser a
customOption s l typ rdr m desc = nullOption $ mconcat
    [short s, long l, metavar typ, eitherReader rdr, help desc, m]

argsOption :: (String -> Maybe a)
           -> Mod ArgumentFields a
           -> String
           -> Parser [a]
argsOption rdr m desc = many . argument rdr $
    metavar "-- ARGS .." <> help desc <> m

roleOption :: Parser Text
roleOption = textOption 'r' "role" mempty
    "Role of the application."

envOption :: Parser Text
envOption = textOption 'e' "env" (value defaultEnv)
    "Environment of the application."

versionOption :: Parser Version
versionOption =
    customOption 'v' "version" "MAJ.MIN.PATCH+BUILD" eitherVersion mempty
        "Version of the application."

keyOption :: Parser FilePath
keyOption = pathOption 'i' "key" (value "")
    "Path to the private key to use."

check :: (MonadIO m, Invalid a) => a -> String -> EitherT AWSError m ()
check x = when (invalid x) . throwT . Err

checkIO :: (MonadIO m, Invalid a) => IO a -> String -> EitherT AWSError m ()
checkIO io e = liftIO io >>= (`check` e)

checkPath :: MonadIO m => FilePath -> String -> EitherT AWSError m ()
checkPath p e = check p msg >> checkIO (not <$> shell (Shell.test_e p)) msg
  where
    msg = Text.unpack (Text.concat ["path '", Shell.toTextIgnore p, "'"]) ++ e
