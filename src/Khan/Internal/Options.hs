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
    , customOption
    , argsOption

    , check
    , checkIO
    , checkPath

    , module Export
    ) where

import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal.Defaults
import           Khan.Internal.IO
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
    <$> switchOption "debug" "Log debug output." False
    <*> readOption "region" "REGION" "Region to operate in." (value NorthCalifornia)
    <*> optional (textOption "iam-profile" "IAM profile to use." mempty)
    <*> stringOption "access-key" "AWS access key." (value "")
    <*> stringOption "secret-key" "AWS secret key." (value "")

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

group :: String -> String -> Mod CommandFields a -> Mod CommandFields a
group name desc cs =
    Options.command name (Options.info (hsubparser cs) (progDesc desc))

command :: Options a
        => String
        -> (Common -> a -> AWS ())
        -> Parser a
        -> String
        -> Mod CommandFields Command
command name f p desc = Options.command name $
    Options.info (Command f <$> p) (progDesc desc)

readOption :: Read a
           => String
           -> String
           -> String
           -> Mod OptionFields a
           -> Parser a
readOption key typ desc m = option $ mconcat
    [long key, metavar typ, reader auto, help desc, m]

switchOption :: String -> String -> Bool -> Parser Bool
switchOption key desc p = flag p (not p) $ long key <> help desc

textOption :: String -> String -> Mod OptionFields Text -> Parser Text
textOption key desc m = nullOption $ mconcat
    [ long key
    , metavar "STR"
    , eitherReader (Right . Text.pack)
    , help desc
    , m
    ]

pathOption :: String -> String -> Mod OptionFields FilePath -> Parser FilePath
pathOption key desc m = nullOption $ mconcat
    [ long key
    , metavar "PATH"
    , eitherReader (Right . Path.decodeString)
    , help desc
    , m
    ]

stringOption :: String -> String -> Mod OptionFields String -> Parser String
stringOption key desc m = strOption $ mconcat
    [ long key
    , metavar "STR"
    , help desc
    , m
    ]

customOption :: String
             -> String
             -> String
             -> (String -> Either String a)
             -> Mod OptionFields a
             -> Parser a
customOption key typ desc rdr m = nullOption $ mconcat
    [long key, metavar typ, eitherReader rdr, help desc, m]

argsOption :: (String -> Maybe a)
           -> String
           -> Mod ArgumentFields a
           -> Parser [a]
argsOption rdr desc m = many . argument rdr $
    metavar "-- ARGS .." <> help desc <> m

check :: (MonadIO m, Invalid a) => a -> String -> EitherT AWSError m ()
check x = when (invalid x) . throwT . Err

checkIO :: (MonadIO m, Invalid a) => IO a -> String -> EitherT AWSError m ()
checkIO io e = liftIO io >>= (`check` e)

checkPath :: MonadIO m => FilePath -> String -> EitherT AWSError m ()
checkPath p e = check p msg >> checkIO (not <$> shell (Shell.test_e p)) msg
  where
    msg = Text.unpack (Text.concat ["path '", Shell.toTextIgnore p, "'"]) ++ e
