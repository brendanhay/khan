{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- Module      : Khan.CLI.Profile
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Profile (commands) where

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Filesystem.Path.CurrentOS  as Path
import           Khan.Internal
import qualified Khan.Model.Profile         as Profile
import           Khan.Prelude
import           Network.AWS.IAM            hiding (Role)
import           Network.HTTP.Types         (urlDecode)

data Role = Role
    { rRole   :: !Text
    , rEnv    :: !Text
    , rTrust  :: !FilePath
    , rPolicy :: !FilePath
    } deriving (Show)

roleParser :: Parser Role
roleParser = Role
    <$> roleOption
    <*> envOption
    <*> pathOption "trust"  (value "") "Trust relationship file."
    <*> pathOption "policy" (value "") "Role policy file."

instance Options Role where
    discover _ Common{..} r@Role{..} = return $! r
        { rTrust  = defaultPath rTrust  trust
        , rPolicy = defaultPath rPolicy policy
        }
      where
        trust  = cConfig </> Path.fromText "trust.json"
        policy = cConfig
            </> "policies"
            </> Path.fromText (policyName $ names r)
            <.> "json"

    validate Role{..} = do
        checkPath rTrust  " specified by --trust must exist."
        checkPath rPolicy " specified by --policy must exist."

instance Naming Role where
    names Role{..} = unversioned rRole rEnv

commands :: Mod CommandFields Command
commands = group "profile" "Long description." $ mconcat
    [ command "info" info roleParser
        "Create or update IAM profiles."
    , command "update" update roleParser
        "Create or update IAM profiles."
    ]

info :: Common -> Role -> AWS ()
info _ r = do
    p <- Profile.find r
    log_ . Text.unlines $
        [ "Arn                  = " <> rArn p
        , "RoleId               = " <> rRoleId p
        , "RoleName             = " <> rRoleName p
        , "CreateDate           = " <> Text.pack (show $ rCreateDate p)
        , "Path                 = " <> rPath p
        , "AssumePolicyDocument = " <> (Text.decodeUtf8
            . LBS.toStrict
            . maybe "" Aeson.encodePretty
            . join
            . fmap policy
            $ rAssumeRolePolicyDocument p)
        ]
  where
    policy :: Text -> Maybe Object
    policy = decode . LBS.fromStrict . urlDecode True . Text.encodeUtf8

update :: Common -> Role -> AWS ()
update _ r = Profile.update r (rPolicy r) (rTrust r)
