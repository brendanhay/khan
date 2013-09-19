{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.Security
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Security (command) where

import           Control.Applicative
import           Control.Error
import           Control.Monad                  (void, when)
import qualified Data.ByteString.Char8          as BS
import           Data.Foldable                  (toList)
import           Data.List                      ((\\))
import           Data.Monoid
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Encoding
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           Network.AWS.EC2.Metadata
import           Network.AWS.Internal.Instances
import           Pipes
import qualified Pipes.Prelude                  as Pipes
import           Text.Show.Pretty

defineOptions "Group" $ do
    textOption "gName" "name" ""
        "A name."

    maybeTextOption "gDesc" "desc" ""
        "An optional description."

    rulesOption "gRules" "rule"
        "An IP permission specification."

deriving instance Show Group

instance Discover Group

instance Validate Group where
    validate Group{..} =
        check gName  "--name must be specified."

command :: Command
command = Command "security" "Manage security groups and rules."
    [ subCommand "describe" describe
    , subCommand "modify"   modify
    , subCommand "delete"   delete
    ]
  where
    describe :: Group -> AWSContext ()
    describe Group{..} = maybe
        (left . Error $ "Unable to find group " ++ Text.unpack gName)
        (logInfo . ppShow) =<< findGroup (Left gName)

    modify Group{..} = do
        logInfo . Text.unpack $ "Searching for group " <> gName <> "..."
        maybe (create >>= update) update =<< findGroup (Left gName)
      where
        create = do
            logInfo . Text.unpack $ gName <> " not found, creating..."
            gid <- fmap csgrGroupId . send $
                CreateSecurityGroup gName (fromMaybe gName gDesc) Nothing
            logInfo . Text.unpack $ "Group " <> gid <> " created."
            g   <- findGroup (Right gid)
            g ?? (Error $ "Unable to find created group " ++ Text.unpack gName)

        update grp = do
            logInfo . Text.unpack $ "Modifying group " <> gName <> "..."

            let gid  = sgitGroupId grp
                strip1 = map (\u -> UserIdGroupPair Nothing Nothing (uigGroupName u))
                strip2 = map (\p -> p { iptGroups = Items . strip1 . toList $ iptGroups p })

                ps   = toList $ sgitIpPermissions grp
                auth = strip2 gRules \\ strip2 ps
                rev  = strip2 ps \\ strip2 gRules

            when (not $ null auth) $ do
                liftIO . putStrLn $ "Authorizing " ++ showRules auth ++ " on " ++ Text.unpack gName ++ "..."
                _ <- send $ AuthorizeSecurityGroupIngress (Just gid) Nothing auth
                return ()

            when (not $ null rev) $ do
                liftIO . putStrLn $ "Revoking " ++ showRules rev ++ " on " ++ Text.unpack gName ++ "..."
                _ <- send $ RevokeSecurityGroupIngress (Just gid) Nothing rev
                return ()

            logInfo . Text.unpack $ "Group " <> gName <> " updated."

    delete Group{..} = do
        logInfo . Text.unpack $ "Deleting group " <> gName <> "..."
        void . send $ DeleteSecurityGroup (Just gName) Nothing
        logInfo "Group deleted."

findGroup :: Either Text Text -> AWSContext (Maybe SecurityGroupItemType)
findGroup eid = group <$> send' (req eid)
  where
    req (Left name) = DescribeSecurityGroups [name] []    []
    req (Right gid) = DescribeSecurityGroups []     [gid] []

    group (Left  _) = Nothing
    group (Right x) = headMay . toList $ dshrSecurityGroupInfo x

