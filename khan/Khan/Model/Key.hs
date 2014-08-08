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

import qualified Data.Text                 as Text
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import qualified Khan.Model.S3.Object      as Object
import           Khan.Prelude              hiding (min, max)
import           Network.AWS.EC2           hiding (Instance)
import qualified Shelly                    as Shell

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
        shell $ do
             p <- Shell.test_e f
             when p $ do
                 ts :: Integer <- truncate <$> liftIO getPOSIXTime
                 Shell.mv f $ f <.> Text.pack (show ts)
             Shell.mkdir_p $ Path.parent f
             Shell.writefile f $ ckqKeyMaterial k
             Shell.run_ "chmod" ["0600", Shell.toTextIgnore f]
        say "Wrote new Key Pair to {}" [f]
        let key = Text.pack . Path.encodeString $ Path.filename f
         in void $ Object.upload b key f True

path :: Naming a => RKeysBucket -> a -> LKeysDir -> AWS FilePath
path (RKeysBucket b) (names -> n@Names{..}) (LKeysDir dir) = do
    f <- filePath n dir
    let key = Text.pack . Path.encodeString $ Path.filename f
     in void $ Object.download b key f False
    shell $ Shell.run_ "chmod" ["0600", Shell.toTextIgnore f]
    return f

filePath :: Names -> FilePath -> AWS FilePath
filePath Names{..} dir = do
    r <- Text.pack . show <$> getRegion
    return $ dir </> Shell.fromText (Text.concat [r, "_", keyName, ".pem"])