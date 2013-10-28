{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Internal.AWS
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.AWS where

import           Control.Exception
import           Control.Monad.Error
import qualified Data.Map                as Map
import qualified Data.Text               as Text
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.Lazy          as LText
import           Khan.Internal.Parsing
import           Khan.Internal.Types
import           Khan.Prelude            hiding (min, max)
import           Network.AWS
import           Network.AWS.AutoScaling hiding (DescribeTags)
import           Network.AWS.EC2 as EC2
import           Network.AWS.IAM
import           Network.Http.Client     hiding (get)

assertAWS :: (MonadError AWSError m, MonadIO m, Params ps)
          => Format
          -> ps
          -> Bool
          -> m ()
assertAWS f ps True  = throwAWS f ps
assertAWS _ _  False = return ()

throwAWS :: (Params a, MonadError AWSError m) => Format -> a -> m b
throwAWS f = throwError . Err . LText.unpack . format f

noteAWS :: (Params ps, MonadError AWSError m)
        => Format
        -> ps
        -> Maybe a
        -> m a
noteAWS f ps = hoistError . note (Err . LText.unpack $ format f ps)

sshGroup :: Text -> Text
sshGroup = (<> "-ssh")

sshRules :: [IpPermissionType]
sshRules = [IpPermissionType TCP 22 22 [] [IpRange "0.0.0.0/0"]]

envTag, roleTag, domainTag, nameTag, versionTag :: Text
envTag     = "Env"
roleTag    = "Role"
domainTag  = "Domain"
nameTag    = "Name"
versionTag = "Version"

defaultTags :: Names -> Text -> [(Text, Text)]
defaultTags Names{..} dom =
    [ (roleTag, roleName)
    , (envTag, envName)
    , (domainTag, dom)
    ] ++ maybe [] (\v -> [(versionTag, v)]) versionName

findRequiredTags :: Text -> AWS Tags
findRequiredTags iid = do
    log "Describing tags for instance-id {}..." [iid]
    send (DescribeTags [TagResourceId [iid]]) >>= lookupTags . tags
  where
    tags = map (\TagSetItemType{..} -> (tsitKey, tsitValue)) . dtagsrTagSet

lookupTags :: (Applicative m, MonadError AWSError m) => [(Text, Text)] -> m Tags
lookupTags (Map.fromList -> ts) = Tags
    <$> require roleTag ts
    <*> require envTag ts
    <*> require domainTag ts
    <*> pure (join $ parseSafeVersionM <$> Map.lookup versionTag ts)
  where
    require k m = hoistError . note (message k m) $ Map.lookup k m

    message k m = Err . Text.unpack $
        Text.concat ["No tag '", k, "' found in [", render m, "]"]

    render = Text.intercalate ","
        . map (\(k, v) -> Text.concat [k, "=", v])
        . Map.toList

verifyAS :: Text -> Either AutoScalingErrorResponse a -> AWS ()
verifyAS  = (`verify` (aseCode . aserError))

verifyEC2 :: Text -> Either EC2ErrorResponse a -> AWS ()
verifyEC2 = (`verify` (ecCode . head . eerErrors))

verifyIAM :: Text -> Either IAMError a -> AWS ()
verifyIAM = (`verify` (etCode . erError))

verify :: (MonadError AWSError m, Eq a, ToError e)
       => a
       -> (e -> a)
       -> Either e b
       -> m ()
verify k f = g
  where
    g (Right _) = return ()
    g (Left  x) | k == f x  = return ()
                | otherwise = throwError $ toError x

isEC2 :: (Functor m, MonadIO m) => m Bool
isEC2 = isRight <$> runEitherT (syncIO attempt)
  where
    attempt = bracket (establishConnection host) closeConnection $ \c -> do
        rq <- buildRequest $ http GET "/latest"
        sendRequest c rq emptyBody

    host = "http://instance-data"
