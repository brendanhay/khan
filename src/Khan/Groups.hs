{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Groups
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Groups where

import           Control.Concurrent.Async (Async)
import           Control.Monad.IO.Class
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           System.Directory

sshGroup :: Text
sshGroup = "ssh"

sshPort :: Integer
sshPort = 22

createGroup :: Text -> Maybe Integer -> AWS ()
createGroup name port = create >> authorise port
  where
    create = check "InvalidGroup.Duplicate" =<<
        sendCatch (CreateSecurityGroup name name Nothing)

    authorise Nothing  = return ()
    authorise (Just n) = check "InvalidPermission.Duplicate" =<<
        sendCatch (AuthorizeSecurityGroupIngress Nothing (Just name)
            [ IpPermissionType TCP n n
                [UserIdGroupPair Nothing Nothing (Just name)] []
            ])

    check k = checkError ((k ==) . ecCode . head . eerErrors)
