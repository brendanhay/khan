{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.Model.S3.Bucket
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.S3.Bucket
    ( download
    , prune
    ) where

import           Control.Arrow
import           Data.Conduit
import qualified Data.Conduit.List         as Conduit
import           Data.Function             (on)
import qualified Data.List                 as List
import           Data.Ord
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import qualified Khan.Model.S3.Object      as Object
import           Khan.Prelude
import           Network.AWS.S3
import qualified Filesystem as FS

download :: Int        -- ^ Concurrency
         -> Text       -- ^ Bucket
         -> Maybe Text -- ^ Prefix
         -> FilePath   -- ^ Destination dir.
         -> Bool       -- ^ Force download if file exists?
         -> Bool       -- ^ Strip prefix from key?
         -> Bool       -- ^ Skip ETag verification if file exists?
         -> AWS Bool
download n b p dir force sdest noVerify = do
    say "Paginating Bucket {} contents" [b]
    or <$> (paginate start
        $= Conduit.concatMap (filter match . gbrContents)
        $= chunked []
        $= Conduit.mapM (mapM (async . retrieve))
        $= Conduit.concatMapM (mapM wait)
        $$ Conduit.consume)
  where
    start = GetBucket b (Delimiter '/') (prefix p) 250 Nothing

    prefix (Just x) = Just . fromMaybe x $ Text.stripPrefix "/" x
    prefix Nothing  = Nothing

    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = True

    removeFromDest True (Just prf) t = fromMaybe t (Text.stripPrefix prf t)
    removeFromDest _    _          t = t

    retrieve Contents{..} = do
        let dest = dir </> Path.fromText (removeFromDest sdest (prefix p) bcKey)
        liftAWS . FS.createTree $ Path.parent dest
        Object.download b bcKey dest force noVerify

    chunked xs = do
        mx <- await
        case mx of
            Just x | length xs < (n - 1) -> chunked (x : xs)
            Just x                       -> yield (x : xs) >> chunked []
            Nothing                      -> void $ yield xs

prune :: Int -> Text -> Maybe Text -> Text -> AWS Bool
prune c b p a = do
    say "Pruning Artifact {} from Bucket {}/{}" [a, b, fromMaybe "" p]
    mk <- paginate start
        $= Conduit.concatMap (split . filter match . gbrContents)
        $$ Conduit.consume
    or <$> (mapM (Object.delete b . fst) . drop c $ sortSnd mk)
  where
    start = GetBucket b (Delimiter '/') (prefix p) 250 Nothing

    prefix (Just x) = Just . suffixed "/" . fromMaybe x $ Text.stripPrefix "/" x
    prefix Nothing  = Nothing
    suffixed s x = if s `Text.isSuffixOf` x then x else x <> s

    match Contents{..}
        | bcStorageClass == Glacier = False
        | isArtifact bcKey          = True
        | otherwise                 = False

    isArtifact = Text.isPrefixOf $ fromMaybe "" (prefix p) <> a <> "_"

    split =
        filter (isJust . snd) . map (second fileNameVersion . join (,) . bcKey)

    sortSnd = List.sortBy (compare `on` (Down . snd))
