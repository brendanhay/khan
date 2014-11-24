{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Khan.Model.S3.Object
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.S3.Object
    ( download
    , upload
    , latest
    , delete
    ) where

import           Control.Arrow
import           Crypto.Hash               (Digest, MD5)
import           Crypto.Hash.Conduit       (hashFile)
import           Data.Byteable             (toBytes)
import qualified Data.ByteString.Base16    as B16
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.List         as Conduit
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Types        (urlEncode)
import           Network.HTTP.Types.Status (status304)

-- | Downloads a file from S3.
-- If the file exists locally and is <= 5GB, a conditional GET
-- request with an ETag is made to ensure the local file is up-to-date.
download :: Text     -- ^ Bucket
         -> Text     -- ^ Key
         -> FilePath -- ^ Destination
         -> Bool     -- ^ Force download even if file exists?
         -> Bool     -- ^ Skip ETag verification if file exists?
         -> AWS Bool
download b k f force noVerify = do
    exists <- liftIO $ FS.isFile f
    size   <- bool (return Nothing) (liftIO (Just <$> FS.getSize f)) exists
    if exists && (noVerify || size > Just etagThreshold)
        then say "File {} already exists" [B f] >> return False
        else do
            et <- if exists && not force
                then pure . ifNoneMatch <$> (hashFile file :: AWS (Digest MD5))
                else return []
            rs <- send $ GetObject b (safeKey k) et
            if responseStatus rs == status304
                then say "File {} already exists" [B f] >> return False
                else do
                    say "Downloading {}/{} to {}" [P b, P k, B f]
                    responseBody rs $$+- Conduit.sinkFile file
                    return True
  where
    file = Path.encodeString f
    -- Files > 5 GB have an ETag which doesn't solely consist of the MD5
    etagThreshold = 5000000000
    ifNoneMatch x = ("If-None-Match",  B16.encode $ toBytes x)

upload :: Text -> Text -> FilePath -> Bool -> AWS Bool
upload b k (Path.encodeString -> f) force = do
    p <- fmap isRight . sendCatch $ HeadObject b (safeKey k) []
    when p $ say "Object {}/{} already exists." [b, k]
    when (not p || force) $ do
        say "Uploading {} to {}/{}" [Text.pack f, b, k]
        mb  <- requestBodyFile f
        bdy <- noteAWS "Unable to get file size: {}" [B f] mb
        send_ $ PutObject b (safeKey k) [] bdy
    return $ not p || force

latest :: Text -> Text -> FilePath -> Bool -> AWS Bool
latest b p f force = do
    say "Paginating Bucket {} contents" [b]
    mk <- paginate start
        $= Conduit.concatMap contents
        $$ Conduit.fold max' Nothing
    maybe (throwAWS "No semantically versioned keys in Bucket {}" [B b])
          (\(k, _) -> download b k f force False)
          mk
  where
    start  = GetBucket b (Delimiter '/') (Just prefix) 250 Nothing
    prefix = fromMaybe p $ Text.stripPrefix "/" p

    contents =
        map (Just . second fileNameVersion . join (,) . bcKey) . gbrContents

    max' (Just x) (Just y)
        | snd y > snd x = Just y
        | otherwise     = Just x
    max' _ y = y

delete :: Text -> Text -> AWS Bool
delete b k = do
    say "Deleting {}/{}" [b, k]
    send_ $ DeleteObject b (safeKey k) []
    return True

safeKey :: Text -> Text
safeKey x = Text.decodeUtf8
    . urlEncode True
    . Text.encodeUtf8
    . fromMaybe x
    $ Text.stripPrefix "/" x
