{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# LANGUAGE ScopedTypeVariables   #-}

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
    , integralOption
    , customOption
    , argsOption

    , roleOption
    , envOption
    , versionOption
    , keyOption
    , trustOption
    , policyOption
    , ansibleOption
    , userOption

    , check
    , checkIO
    , checkPath

    , module Export
    ) where

import qualified Data.Attoparsec.Text      as AText
import           Data.SemVer
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal.IO
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import           Options.Applicative       as Export hiding (command, info)
import qualified Options.Applicative       as Options
import           Prelude                   (error)
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
    <*> lookupEnv "KHAN_RKEYS" "" Text.pack
         (textOption "remote-keys" (value "" <> short 'K')
        "Bucket to retrieve/store certificates.")
    <*> lookupEnv "KHAN_LKEYS" "/etc/ssl/khan" Path.decodeString
        (pathOption "local-keys" (value "" <> short 'L')
        "Path to certificates.")
    <*> lookupEnv "KHAN_CACHE" "/var/cache/khan" Path.decodeString
        (pathOption "cache" (value "")
        "Path to cache.")
    <*> lookupEnv "KHAN_CONFIG" "/etc/khan" Path.decodeString
        (pathOption "config" (value "")
        "Path to configuration files.")
  where
    lookupReg :: String -> Parser Region -> Parser Region
    lookupReg k =
        fmap (fromMaybe (error "--region or KHAN_REGION must be specified."))
             . lookupEnv k Nothing readMay . optional

    lookupEnv :: Invalid a
              => String
              -> a
              -> (String -> a)
              -> Parser a
              -> Parser a
    lookupEnv k d f = fmap g
      where
        g v | valid v   = v
            | otherwise = maybe d f $ k `lookup` env

instance Options Common where
    validate Common{..} = do
       check cBucket     "--remote-keys or KHAN_RKEYS must be specified."
       checkPath cCerts  " specified by --local-keys or KHAN_LKEYS must exist."
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

integralOption :: (Show a, Integral a)
               => String
               -> Mod OptionFields a
               -> String
               -> Parser a
integralOption key = customOption key "INT"
    (AText.parseOnly AText.decimal . Text.pack)

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

roleOption :: Parser Role
roleOption = Role <$> textOption "role" (short 'r')
    "Role of the application."

envOption :: Parser Env
envOption = Env <$> textOption "env" (short 'e')
    "Environment of the application."

versionOption :: Parser Version
versionOption = customOption "version" typ (parseVersion . Text.pack) mempty
    "Version of the application."
  where
    typ = "MAJ.MIN.PATCH[-RELEASE][+BUILD]"

keyOption :: Parser (Maybe FilePath)
keyOption = optional $ pathOption "key" (short 'i')
    "Path to the private key to use."

trustOption :: Parser TrustPath
trustOption = TrustPath <$> pathOption "trust"  (value "")
    "Trust relationship file."

policyOption :: Parser PolicyPath
policyOption = PolicyPath <$> pathOption "policy" (value "")
    "Role policy file."

ansibleOption :: Parser Bool
ansibleOption = switchOption "ansible" False
    "Ansible module compatible output."

userOption :: Parser Text
userOption = textOption "user" (value "ubuntu" <> short 'u')
    "SSH User."

check :: (MonadIO m, Invalid a) => a -> String -> EitherT AWSError m ()
check x = when (invalid x) . throwT . Err

checkIO :: (MonadIO m, Invalid a) => IO a -> String -> EitherT AWSError m ()
checkIO io e = liftIO io >>= (`check` e)

checkPath :: MonadIO m => FilePath -> String -> EitherT AWSError m ()
checkPath p e = check p msg >> checkIO (not <$> shell (Shell.test_e p)) msg
  where
    msg = Text.unpack (Text.concat ["path '", Shell.toTextIgnore p, "'"]) ++ e
