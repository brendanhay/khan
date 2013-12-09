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
    ) where

import qualified Data.Text                 as Text
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS as Path
import           Khan.Internal
import qualified Khan.Model.Object         as Object
import           Khan.Prelude              hiding (min, max)
import           Network.AWS.EC2           hiding (Instance)
import qualified Shelly                    as Shell

create :: Naming a => a -> AWS ()
create (names -> n@Names{..}) =
    sendCatch (CreateKeyPair keyName) >>= either exist write
  where
    exist e = do
        verifyEC2 "InvalidKeyPair.Duplicate" (Left e)
        log "Key Pair {} exists, not updating." [keyName]

    write k = do
        f <- keyPath n
        shell $ do
             p <- Shell.test_e f
             when p $ do
                 ts :: Integer <- truncate <$> liftIO getPOSIXTime
                 Shell.mv f $ f <.> Text.pack (show ts)
             Shell.mkdir_p $ Path.parent f
             Shell.writefile f $ ckqKeyMaterial k
             Shell.run_ "chmod" ["0600", Shell.toTextIgnore f]
        log "Wrote new Key Pair to {}" [f]
--        Object.upload 
