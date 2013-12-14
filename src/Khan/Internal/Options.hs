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
    ( Options (..)
    , Command (..)
    , Common  (..)

    , commonParser

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
    , ansibleOption

    , check
    , checkIO
    , checkPath

    , module Export
    ) where

import qualified Data.Text                 as Text
import           Data.Version
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal.IO
import           Khan.Internal.Parsing
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import           Options.Applicative       as Export hiding (command, info)
import qualified Options.Applicative       as Options
import qualified Shelly                    as Shell

data Common = Common
    { cDebug  :: !Bool
    , cSilent :: !Bool
    , cRegion :: !Region
    , cBucket :: !Text
    , cCerts  :: !FilePath
    , cCache  :: !FilePath
    , cConfig :: !FilePath
    } deriving (Show)

class Options a where
    discover :: Bool -> Common -> a -> AWS a
    validate :: MonadIO m => a -> EitherT AWSError m ()

    discover _ _ = return
    validate     = void . return

data Command where
    Command :: Options a => (Common -> a -> AWS ()) -> a -> Command

commonParser :: [(String, String)] -> Parser Common
commonParser env = Common
    <$> switchOption "debug" False
        "Log debug output."
    <*> switchOption "silent" False
        "Suppress standard log output."
    <*> lookupReg "KHAN_REGION"
        (readOption "region" "REGION" (short 'R')
        "Region to operate in.")
    <*> lookupEnv "KHAN_BUCKET" Text.pack
        (textOption "cert-bucket" (value "")
        "Bucket to retrieve/store certificates.")
    <*> lookupEnv "KHAN_CERTS" Path.decodeString
        (pathOption "certs" (value "certs")
        "Path to certificates.")
    <*> lookupEnv "KHAN_CACHE" Path.decodeString
        (pathOption "cache" (value "cache")
        "Path to cache.")
    <*> lookupEnv "KHAN_CONFIG" Path.decodeString
        (pathOption "config" (value "config")
        "Path to configuration files.")
  where
    lookupReg :: String -> Parser Region -> Parser Region
    lookupReg k =
        fmap (fromMaybe NorthVirginia) . lookupEnv k readMay . optional

    lookupEnv :: Invalid a => String -> (String -> a) -> Parser a -> Parser a
    lookupEnv k f = fmap g
      where
        g v | valid v   = v
            | otherwise = fromMaybe v . fmap f $ k `lookup` env

instance Options Common where
    validate Common{..} = do
       check cBucket     "--cert-bucket or KHAN_BUCKET must be specified."
       checkPath cCerts  " specified by --certs or KHAN_CERTS must exist."
       checkPath cCache  " specified by --cache or KHAN_CACHE must exist."
       checkPath cConfig " specified by --config or KHAN_CONFIG must exist."

group :: String -> String -> Mod CommandFields a -> Mod CommandFields a
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

readOption :: (Show a, Read a)
           => String
           -> String
           -> Mod OptionFields a
           -> String
           -> Parser a
readOption key typ m desc = option $ mconcat
    [long key, metavar typ, reader auto, help desc, m, showDefault]

switchOption :: String -> Bool -> String -> Parser Bool
switchOption key p desc = flag p (not p) $ long key <> help desc

textOption :: String -> Mod OptionFields Text -> String -> Parser Text
textOption key = customOption key "STR" (Right . Text.pack)

pathOption :: String
           -> Mod OptionFields FilePath
           -> String
           -> Parser FilePath
pathOption key = customOption key "PATH" (Right . Path.decodeString)

stringOption :: String
             -> Mod OptionFields String
             -> String
             -> Parser String
stringOption key m desc = strOption $ mconcat
    [long key, metavar "STR", help desc, m, showDefault]

integerOption :: String
              -> Mod OptionFields Integer
              -> String
              -> Parser Integer
integerOption key = readOption key "INT"

customOption :: Show a
             => String
             -> String
             -> (String -> Either String a)
             -> Mod OptionFields a
             -> String
             -> Parser a
customOption key typ rdr m desc = nullOption $ mconcat
    [long key, metavar typ, eitherReader rdr, help desc, m, showDefault]

argsOption :: (String -> Maybe a)
           -> Mod ArgumentFields a
           -> String
           -> Parser [a]
argsOption rdr m desc = many . argument rdr $
    metavar "ARGS .." <> help desc <> m

roleOption :: Parser Text
roleOption = textOption "role" (short 'r')
    "Role of the application."

envOption :: Parser Text
envOption = textOption "env" (value defaultEnv <> short 'e')
    "Environment of the application."

versionOption :: Parser Version
versionOption = customOption "version" "MAJ.MIN.PATCH+BUILD" eitherVersion mempty
    "Version of the application."

keyOption :: Parser (Maybe FilePath)
keyOption = optional $ pathOption "key" (short 'i')
    "Path to the private key to use."

ansibleOption :: Parser Bool
ansibleOption = switchOption "ansible" False "Ansible module compatible output."

check :: (MonadIO m, Invalid a) => a -> String -> EitherT AWSError m ()
check x = when (invalid x) . throwT . Err

checkIO :: (MonadIO m, Invalid a) => IO a -> String -> EitherT AWSError m ()
checkIO io e = liftIO io >>= (`check` e)

checkPath :: MonadIO m => FilePath -> String -> EitherT AWSError m ()
checkPath p e = check p msg >> checkIO (not <$> shell (Shell.test_e p)) msg
  where
    msg = Text.unpack (Text.concat ["path '", Shell.toTextIgnore p, "'"]) ++ e
