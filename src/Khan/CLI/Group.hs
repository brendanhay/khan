{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Group
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Group (commands) where

import qualified Khan.AWS.EC2     as EC2
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS.EC2
import           Text.Show.Pretty

data Group = Group
    { gRole  :: !Text
    , gEnv   :: !Text
    , gRules :: [IpPermissionType]
    } deriving (Show)

groupParser :: Parser Group
groupParser = Group
    <$> roleOption
    <*> envOption
    <*> many (customOption "rule" "RULE" parseRules mempty
        "Rule description.")

instance Options Group where
    validate Group{..} = do
        check gRole "--role must be specified."
        check gEnv  "--env must be specified."

instance Naming Group where
    names Group{..} = unversioned gRole gEnv

commands :: Mod CommandFields Command
commands = group "group" "Long description."
     $ command "info" info groupParser
        "Long long long long description."
    <> command "update" update groupParser
        "Long long long long description."
    <> command "delete" delete groupParser
        "Long long long long description."
  where
    info _ g = do
        mg <- EC2.findGroup g
        liftIO $ maybe (return ()) (putStrLn . ppShow) mg

    update _ g = EC2.updateGroup g [] -- $ gRules g

    delete _ = EC2.deleteGroup
