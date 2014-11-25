{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Model.Key
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Key
    ( create
    , path
    ) where

import           Data.Bits
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import qualified Khan.Model.S3.Object      as Object
import           Khan.Prelude              hiding (min, max)
import           Network.AWS.EC2           hiding (Instance)
import           System.Posix.Files        (ownerReadMode, ownerWriteMode)

create :: Naming a => RKeysBucket -> a -> LKeysDir -> AWS FilePath
create (RKeysBucket b) (names -> n@Names{..}) (LKeysDir dir) = do
    f <- filePath n dir
    e <- sendCatch $ CreateKeyPair keyName
    either exist (write f) e
    return f
  where
    exist e = do
        verifyEC2 "InvalidKeyPair.Duplicate" (Left e)
        void $ path (RKeysBucket b) n (LKeysDir dir)
        say "Key Pair {} exists, not updating." [keyName]

    write f k = do
        writeFile f (ownerReadMode  .|. ownerWriteMode) (ckqKeyMaterial k)
        say "Wrote new Key Pair to {}" [f]
        let key = Text.pack . Path.encodeString $ Path.filename f
        void $ Object.upload b key f True

path :: Naming a => RKeysBucket -> a -> LKeysDir -> AWS FilePath
path (RKeysBucket b) (names -> n@Names{..}) (LKeysDir dir) = do
    f <- filePath n dir
    let key = Text.pack . Path.encodeString $ Path.filename f
    void $ Object.download b key f False False
    setFileMode f (ownerReadMode  .|. ownerWriteMode)
    return f

filePath :: Names -> FilePath -> AWS FilePath
filePath Names{..} dir = do
    r <- Text.pack . show <$> getRegion
    return $ dir </> Path.fromText (Text.concat [r, "_", keyName, ".pem"])
