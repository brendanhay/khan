{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Role
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Role (command) where

import           Control.Applicative
import           Control.Concurrent      (threadDelay)
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable           (toList)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Data.Version
import           Khan.Groups
import           Khan.Internal
import           Khan.Keys
import           Network.AWS
import           Network.AWS.AutoScaling hiding (Filter)
import           Network.AWS.EC2
import           Network.AWS.IAM         hiding (Role)
import           Network.AWS.Internal
import           Pipes
import qualified Pipes.Prelude           as Pipes
import           Text.Show.Pretty

defineOptions "Role" $ do
    textOption "rName" "name" ""
        "Name of the application."

    textOption "rEnv" "env" defaultEnv
        "Environment of the application."

    stringOption "rPolicy" "policy" "./config/role-policy.json"
        "Role policy file."

    stringOption "rTrust" "trust" "./config/trust-relationship.json"
        "Trust relationship file."

deriving instance Show Role

instance Discover Role

instance Validate Role where
    validate Role{..} = do
        check rName "--name must be specified."
        check rEnv  "--env must be specified."
        checkPath rPolicy $ rPolicy ++ " specified by --policy must exist."
        checkPath rTrust $ rTrust ++ " specified by --trust must exist."

defineOptions "Search" $ return ()

deriving instance Show Search

instance Discover Search
instance Validate Search

command :: Command
command = Command "role" "Manage IAM Roles."
    [ subCommand "create" create
    , subCommand "delete" delete
    , subCommand "search" search
    ]
  where
    create Role{..} = do
        (policy, trust) <- liftIO $ (,)
            <$> Text.readFile rPolicy
            <*> Text.readFile rTrust

        i <- sendAsync $ CreateInstanceProfile role Nothing
        r <- sendAsync $ CreateRole trust Nothing role

        wait i >>= checkError (("EntityAlreadyExists" ==) . etCode . erError)
        wait r >>= checkError (("EntityAlreadyExists" ==) . etCode . erError)

        a <- sendAsync $ AddRoleToInstanceProfile role role
        p <- sendAsync $ PutRolePolicy policy role role

        wait a >>= checkError (("LimitExceeded" ==) . etCode . erError)
        waitAsync_ p <* logInfo "Updated policy for role {}" [role]
      where
        role = Text.concat [rName, "-", rEnv]

    delete Role{..} = return ()

    search Search{..} = runEffect $ for (paginate start) (liftIO . display)
      where
        start = ListRoles Nothing Nothing Nothing

        display (toList . lrrRoles . lrrListRolesResult -> rs) =
            unless (null rs) $ do
                mapM_ (logInfo_ . Text.pack . ppShow) rs
                logInfo_ "Press enter to continue..." >> void getLine


-- Centralise name concat into Formatters.hs
