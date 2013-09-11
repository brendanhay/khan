{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE Rank2Types     #-}

-- Module      : Khan.Firewall
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Firewall (firewall) where

import           Control.Applicative
import           Control.Error
import           Control.Monad            (void)
import qualified Data.ByteString.Char8    as BS
import           Data.Foldable            (forM_)
import qualified Data.Text                as Text
import           Data.Text.Encoding
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
import           Network.AWS.EC2.Metadata
import           Pipes
import qualified Pipes.Prelude            as Pipes
import           Text.Show.Pretty

defineOptions "Group" $ do
    textsOption "gNames" "name" []
        "A name to search for."

    regionOption "gRegion" "region" Ireland
        "Region of the group."

deriving instance Show Group

instance Discover Group

instance Validate Group where
    validate _ = return ()

-- defineOptions "Rule" $ do
--     return ()

-- deriving instance Show Rule

-- instance Validate Rule

firewall :: Command
firewall = Command "firewall" "Manage firewall groups and rules."
    [ subCommand "search" search
    , subCommand "create" create
    ]
  where
    search Group{..} = do
        logInfo $ "Searching for groups matching '"
            <> Text.intercalate "," gNames
            <> "'..."
        within gRegion $ do
            res <- send $ DescribeSecurityGroups gNames [] []
            forM_ (dshrSecurityGroupInfo res) $ \g -> liftIO $ do
                logInfo $ ppShow g
                logInfo "Press enter to continue..."
                void getLine

    create 
