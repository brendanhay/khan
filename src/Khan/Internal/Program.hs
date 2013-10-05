{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- Module      : Khan.Internal.Program
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Program
    (
    -- * Application Options
      Khan     (..)
    , Command  (..)
    , SubCommand

    -- * Program Helpers
    , Discover (..)
    , Validate (..)
    , defineOptions
    , check
    , checkIO
    , checkPath
    , runProgram
    , subCommand
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8     as BS
import           Data.List                 (find)
import qualified Data.Text                 as Text
import           Data.Text.Encoding
import           Khan.Internal.Log
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Network.AWS
import           Network.AWS.EC2.Metadata
import           Options                   hiding (boolOption, stringOption)
import           System.Directory
import           System.Environment
import           System.Exit

accessKey, secretKey :: String
accessKey = "ACCESS_KEY_ID"
secretKey = "SECRET_ACCESS_KEY"

defineOptions "Khan" $ do
    boolOption "kDebug" "debug" False
        "Log debug output."

    boolOption "kDiscover" "discover" True
        "Populate options from EC2 tags and metadata (if possible)."

    maybeTextOption "kRole" "iam-role" Text.empty
        "IAM role - if specified takes precendence over access/secret keys."

    stringOption "kAccess" "access-key" ""
        "AWS access key."

    stringOption "kSecret" "secret-key" ""
        "AWS secret key."

    regionOption "kRegion" "region" Ireland
        "Region to operate in."

deriving instance Show Khan

initialise :: MonadIO m => Khan -> EitherT Error m Khan
initialise k@Khan{..}
    | isJust kRole = right k
    | validKeys k  = right k
    | otherwise    = lookupKeys
  where
    lookupKeys = fmapLT toError . syncIO $ do
        acc <- env accessKey kAccess
        sec <- env secretKey kSecret
        return $! k { kAccess = acc, kSecret = sec }

    env k' v
        | null v    = fromMaybe "" <$> lookupEnv k'
        | otherwise = return v

instance Discover Khan where
    discover k@Khan{..} = liftEitherT $ do
        ec2 <- doesMetadataExist
        if ec2 then getRegion else return k
      where
        getRegion = do
            az  <- BS.unpack . BS.init <$> metadata AvailabilityZone
            reg <- fmapLT toError $
                tryRead ("Failed to read region from: " ++ az) az
            return $! k { kRegion = reg }

instance Validate Khan where
    validate Khan{..} = do
        check (null kAccess && role) $ msg "--access-key" accessKey
        check (null kSecret && role) $ msg "--secret-key" secretKey
      where
        role = isNothing kRole

        msg k e = k ++ " must be specified or "
            ++ e ++ " env must be set if --iam-role is not set."

validKeys :: Khan -> Bool
validKeys Khan{..} = (not . null) `all` [kAccess, kSecret]

check :: (Monad m, Invalid a) => a -> String -> EitherT Error m ()
check x = when (invalid x) . throwT . Error

checkIO :: (MonadIO m, Invalid a) => IO a -> String -> EitherT Error m ()
checkIO io e = liftIO io >>= (`check` e)

checkPath :: MonadIO m => FilePath -> String -> EitherT Error m ()
checkPath p = checkIO (not <$> doesFileExist p)

type SubCommand = Subcommand Khan (EitherT Error IO ())

subCommand :: (Show a, Options a, Discover a, Validate a)
           => String
           -> (a -> AWS ())
           -> SubCommand
subCommand name action = Options.subcommand name run
  where
    run k o _ = do
        khan@Khan{..} <- initialise k
        validate khan
        logInfo "Setting region to {}..." [Shown kRegion]
        env <- Env (Just kRegion) kDebug <$> credentials (creds khan)
        res <- lift . runAWS env $ do
            opts <- disco kDiscover o
            liftEitherT $ validate opts
            action opts
        hoistEither res

    disco True  = (logDebug_ "Performing discovery..." >>) . discover
    disco False = (logDebug_ "Skipping discovery..."   >>) . return

    creds Khan{..}
        | Just r <- kRole = FromRole $! encodeUtf8 r
        | otherwise       = FromKeys (BS.pack kAccess) (BS.pack kSecret)

data Command = Command
    { cmdName :: String
    , cmdDesc :: String
    , cmdSubs :: [SubCommand]
    }

runProgram :: [Command] -> [Command] -> IO ()
runProgram wflow metal = do
    args <- getArgs
    case args of
        []          -> help "Additional subcommand required."
        (name:rest) -> maybe (help $ "Unknown subcommand \"" ++ name ++ "\".")
            (run rest . cmdSubs) $ find ((== name) . cmdName) cmds
  where
    cmds = wflow ++ metal

    run argv subs =
        let parsed = parseSubcommand subs argv
        in case parsedSubcommand parsed of
            Just cmd -> runScript $ fmapLT awsError cmd <* logInfo_ "Exiting..."
            Nothing  -> case parsedError parsed of
                Just ex -> do
                    putStrLn $ parsedHelp parsed
                    putStrLn ex
                    exitFailure
                Nothing -> do
                    putStrLn $ parsedHelp parsed
                    exitSuccess

    help msg = do
        putStrLn $ parsedHelp (parseOptions [] :: ParsedOptions Khan)
        putStrLn $ unlines ("Ephemeral:" : map desc wflow ++ [""])
        putStrLn $ unlines ("Persistent:" : map desc metal ++ [""])
        putStrLn msg
        exitFailure

    desc Command{..} =
        "  " ++ cmdName ++ replicate (28 - length cmdName) ' ' ++ cmdDesc

    awsError (Error s) = s
    awsError (Ex e)    = show e
