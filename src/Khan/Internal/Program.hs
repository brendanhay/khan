{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

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
      Khan    (..)
    , Command (..)

    -- * Validation
    , check
    , checkIO
    , checkPath

    -- * Program Helpers
    , defineOptions
    , runProgram
    , command
    ) where

import qualified Data.ByteString.Char8    as BS
import qualified Data.Text                as Text
import           Data.Text.Encoding
import           Data.Text.Format         (Shown(..))
import qualified Data.Text.Format         as Text
import qualified Data.Text.Lazy           as LText
import           Khan.Internal.AWS
import           Khan.Internal.IO
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata
import           Options                  hiding (group, boolOption, stringOption)
import qualified Shelly                   as Shell
import           System.Environment
import           System.Exit
import           Text.Regex

accessKey, secretKey :: String
accessKey = "ACCESS_KEY_ID"
secretKey = "SECRET_ACCESS_KEY"

defineOptions "Khan" $ do
    boolOption "kDebug" "debug" False
        "Log debug output."

    maybeTextOption "kRole" "iam-role" ""
        "IAM role - if specified takes precendence over access/secret keys."

    stringOption "kAccess" "access-key" ""
        "AWS access key."

    stringOption "kSecret" "secret-key" ""
        "AWS secret key."

    regionOption "kRegion" "region" NorthCalifornia
        "Region to operate in."

    boolOption "kDisco" "discovery" False
        "Discovery of command line parameters from EC2 metadata."

-- FIXME: Discovery should only be for EC2 metadata

deriving instance Show Khan

initialise :: (Applicative m, MonadIO m) => Khan -> EitherT AWSError m Khan
initialise k@Khan{..}
    | isJust kRole = right k
    | validKeys k  = right k
    | otherwise    = lookupKeys
  where
    lookupKeys = fmapLT toError . syncIO $ do
        acc <- env accessKey kAccess
        sec <- env secretKey kSecret
        return $! k { kAccess = acc, kSecret = sec }
      where
        env k' v
            | null v    = fromMaybe "" <$> lookupEnv k'
            | otherwise = return v

instance Validate Khan where
    validate Khan{..} = do
        check (null kAccess && role) $ msg "--access-key" accessKey
        check (null kSecret && role) $ msg "--secret-key" secretKey
      where
        role = isNothing kRole

        msg k e = concat
            [ k
            , " must be specified or "
            , e
            , " env must be set if --iam-role is not set."
            ]

data Command = Command
    { cmdSub  :: Subcommand Khan (EitherT AWSError IO ())
    , cmdName :: String
    , cmdDesc :: String
    , cmdHelp :: String
    }

validKeys :: Khan -> Bool
validKeys Khan{..} = (not . null) `all` [kAccess, kSecret]

check :: (Monad m, Invalid a) => a -> String -> EitherT AWSError m ()
check x = when (invalid x) . throwT . Err

checkIO :: (MonadIO m, Invalid a) => IO a -> String -> EitherT AWSError m ()
checkIO io e = liftIO io >>= (`check` e)

checkPath :: MonadIO m => FilePath -> String -> EitherT AWSError m ()
checkPath p e = check p msg >> checkIO (not <$> shell (Shell.test_e p)) msg
  where
    msg = Text.unpack (Text.concat ["path '", path p, "'"]) ++ e

runProgram :: [Command] -> IO ()
runProgram cmds = do
    args <- getArgs
    let p = parseSubcommand (map cmdSub cmds) args
    case parsedSubcommand p of
        Just cmd -> runScript $ fmapLT show cmd <* log_ "Exiting..."
        Nothing  -> do
            maybe (return ()) synopsis $ headMay args
            help p
            maybe exitSuccess (\ex -> putStrLn ex >> exitFailure) $
                parsedError p
  where
    help p = putStrLn . init $ foldl replace (parsedHelp p) cmds

    replace s Command{..} = subRegex (mkRegex $ "^  " ++ cmdName) s
        . LText.unpack
        $ Text.format "  {}{}{}" [cmdName, pad cmdName, cmdDesc]

    pad = flip replicate ' ' . (28 -) . length

    synopsis name = maybe (return ()) (putStrLn . about) $
        find ((name ==) . cmdName) cmds

    about Command{..} = unlines $
        [ "Synopsis:"
        , concat ["  ", cmdName, " - ", cmdDesc]
        , ""
        ] ++ map (' ':) (wrapLines 80 cmdHelp)

command :: (Show a, Options a, Discover a, Validate a)
         => (a -> AWS ())
         -> String
         -> String
         -> String
         -> Command
command action name = Command (Options.subcommand name run) name
  where
    run khan opts _ = do
        k@Khan{..} <- initialise =<< regionalise khan
        validate k
        log "Setting region to {}..." [Shown kRegion]
        r <- lift . runAWS (creds k) kDebug . within kRegion $ do
            o <- disco kDisco opts
            liftEitherT $ validate o
            action o
        hoistEither r

    regionalise k = do
        p <- isEC2
        if not p
            then return k
            else do
                az  <- BS.unpack . BS.init <$> metadata AvailabilityZone
                reg <- fmapLT Err $
                    tryRead ("Failed to read region from: " ++ az) az
                return $! k { kRegion = reg }

    creds Khan{..}
        | Just r <- kRole = FromRole $! encodeUtf8 r
        | otherwise       = FromKeys (BS.pack kAccess) (BS.pack kSecret)

    disco True  = (log_ "Performing discovery..." >>) . discover
    disco False = (log_ "Skipping discovery..." >>) . return

wrapLines :: Int -> String -> [String]
wrapLines n s = reverse . uncurry (:) $ foldl f ([], []) $ words s
  where
    f (ws, ls) w
        | length ws < n = (concat [ws, " ", w], ls)
        | otherwise     = ([], ws : ls)
