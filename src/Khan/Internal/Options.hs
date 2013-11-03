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

    , define
    , manyOptions
    , switchOption

    , textOption
    , maybeTextOption
    , pathOption
    , stringOption
    , readOption

    , group
    , command

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
    { optDebug   :: !Bool
    , optRegion  :: !Region
    , optProfile :: Maybe Text
    , optAccess  :: !String
    , optSecret  :: !String
    } deriving (Show)

commonParser :: Parser Common
commonParser = Common
    <$> switchOption "debug" False "Log debug output."
    <*> readOption "region" "REGION" NorthCalifornia "Region to operate in."
    <*> maybeTextOption "iam-profile" Nothing "IAM profile to use."
    <*> stringOption "access-key" "" "AWS access key."
    <*> stringOption "secret-key" "" "AWS secret key."

instance Options Common where
    validate Common{..} = do
        check (null optAccess && profile) $ msg "--access-key" accessKey
        check (null optSecret && profile) $ msg "--secret-key" secretKey
      where
        profile = isNothing optProfile
        msg k e = concat
            [ k
            , " must be specified or "
            , e
            , " env must be set if --iam-role is not set."
            ]

initialise :: (Applicative m, MonadIO m) => Common -> EitherT AWSError m Common
initialise o@Common{..}
    | isJust optProfile = right o
    | validKeys         = right o
    | otherwise         = lookupKeys
  where
    validKeys = (not . null) `all` [optAccess, optSecret]

    lookupKeys = fmapLT toError . syncIO $ do
        acc <- env accessKey optAccess
        sec <- env secretKey optSecret
        return $! o { optAccess = acc, optSecret = sec }
      where
        env k v
            | null v    = fromMaybe "" <$> lookupEnv k
            | otherwise = return v

define :: (String -> a)
       -> String
       -> String
       -> a
       -> String
       -> Parser a
define rdr key typ val desc = nullOption $
    long key <> metavar typ <> eitherReader (Right . rdr) <> value val <> help desc

readOption :: Read a
           => String
           -> String
           -> a
           -> String
           -> Parser a
readOption key typ val desc = option $
    long key <> metavar typ <> reader auto <> value val <> help desc

manyOptions :: (String -> Either String a)
           -> String
           -> String
           -> String
           -> Parser [a]
manyOptions rdr key typ desc = many . nullOption $
     long key <> metavar typ <> eitherReader rdr <> help desc

switchOption :: String -> Bool -> String -> Parser Bool
switchOption key val desc = flag val (not val) $ long key <> help desc

textOption :: String -> Text -> String -> Parser Text
textOption key = define Text.pack key "STR"

maybeTextOption :: String -> Maybe Text -> String -> Parser (Maybe Text)
maybeTextOption key = define f key "STR"
  where
    f x = if null x then Nothing else Just $ Text.pack x

pathOption :: String -> FilePath -> String -> Parser FilePath
pathOption key = define Path.decodeString key "PATH"

stringOption :: String -> String -> String -> Parser String
stringOption key = define id key "STR"

group :: String -> String -> Mod CommandFields a -> Mod CommandFields a
group name desc cs =
    Options.command name (Options.info (hsubparser cs) (progDesc desc))

command :: Options a
        => String
        -> (Common -> a -> AWS ())
        -> Parser a
        -> String
        -> Mod CommandFields Command
command name f p desc =
    Options.command name (Options.info (Command f <$> p) (progDesc desc))

check :: (MonadIO m, Invalid a) => a -> String -> EitherT AWSError m ()
check x = when (invalid x) . throwT . Err

checkIO :: (MonadIO m, Invalid a) => IO a -> String -> EitherT AWSError m ()
checkIO io e = liftIO io >>= (`check` e)

checkPath :: MonadIO m => FilePath -> String -> EitherT AWSError m ()
checkPath p e = check p msg >> checkIO (not <$> shell (Shell.test_e p)) msg
  where
    msg = Text.unpack (Text.concat ["path '", Shell.toTextIgnore p, "'"]) ++ e
