{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Artifact
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Artifact (commands) where

import           Khan.Internal        hiding (sync)
import           Khan.Model.Ansible
import qualified Khan.Model.S3.Bucket as Bucket
import qualified Khan.Model.S3.Object as Object
import           Khan.Prelude
import           Network.AWS.S3       (AWS)

data Object = Object
    { oBucket  :: !Text
    , oKey     :: !Text
    , oPath    :: !FilePath
    , oForce   :: !Bool
    , oAnsible :: !Bool
    } deriving (Show)

objectParser :: Parser Object
objectParser = Object
    <$> textOption "bucket" (short 'b')
        "S3 bucket the object resides in."
    <*> textOption "key" (short 'k')
        "Full S3 key prefix of the object relative to the bucket."
    <*> pathOption "file" (short 'f' <> action "file")
        "Path to the local destination file."
    <*> switchOption "force" False
        "Overwrite the local destination if it already exists."
    <*> ansibleOption

instance Options Object

data Bucket = Bucket
    { bBucket  :: !Text
    , bPrefix  :: Maybe Text
    , bDir     :: !FilePath
    , bN       :: !Int
    , bForce   :: !Bool
    , bAnsible :: !Bool
    } deriving (Show)

bucketParser :: Parser Bucket
bucketParser = Bucket
    <$> textOption "bucket" (short 'b')
        "S3 bucket the object resides in."
    <*> optional (textOption "prefix" (short 'p')
        "Optional S3 key partial prefix relative to the bucket.")
    <*> pathOption "dir" (short 'd' <> action "directory")
        "Destination directory to write objects into."
    <*> readOption "concurrency" "INT" (short 'n' <> value 4)
        "Number of simultaneous downloads."
    <*> switchOption "force" False
        "Overwrite the destination object if it already exists."
    <*> ansibleOption

instance Options Bucket

data Prune = Prune
    { pBucket   :: !Text
    , pPrefix   :: Maybe Text
    , pArtifact :: !Text
    , pCopies   :: !Int
    , pAnsible  :: !Bool
    } deriving (Show)

pruneParser :: Parser Prune
pruneParser = Prune
    <$> textOption "bucket" (short 'b')
        "Bucket."
    <*> optional (textOption "prefix" (short 'p')
        "Prefix.")
    <*> textOption "artifact" (short 'a')
        "Artifact to prune."
    <*> readOption "copies" "INT" (short 'c' <> value 5)
        "Number of copies to retain."
    <*> ansibleOption

instance Options Prune

commands :: Mod CommandFields Command
commands = group "artifact" "Manage S3 Artifacts." $ mconcat
    [ command "upload" (object Object.upload) objectParser
        "Upload an object to S3."
    , command "download" (object Object.download) objectParser
        "Download an object from S3 to disk."
    , command "latest" (object Object.latest) objectParser
        "Download the latest semantically versioned object to disk."
    , command "sync" sync bucketParser
        "Synchronize a bucket to disk."
    , command "prune" prune pruneParser
        "Prune old artifacts from the bucket."
    ]

object :: (Text -> Text -> FilePath -> Bool -> AWS Bool)
       -> Common
       -> Object
       -> AWS ()
object g c Object{..} =
    capture oAnsible c "object {}/{}" [oBucket, oKey] $
        g oBucket oKey oPath oForce

sync :: Common -> Bucket -> AWS ()
sync c Bucket{..} =
    capture bAnsible c "bucket {}/{}" [bBucket, fromMaybe "" bPrefix] $
        Bucket.download bN bBucket bPrefix bDir bForce

prune :: Common -> Prune -> AWS ()
prune c Prune{..} =
    capture pAnsible c "bucket {}/{} {}" [pBucket, fromMaybe "" pPrefix, pArtifact] $
        Bucket.prune pCopies pBucket pPrefix pArtifact
