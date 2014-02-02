{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
import           Khan.Internal
import           Khan.Model.Profile         (Policy(..))
import qualified Khan.Model.Profile         as Profile
import           Khan.Prelude
import           Network.AWS.IAM            hiding (Role)
import           Network.HTTP.Types         (urlDecode)

default (Text)

data Profile = Profile
    { rRole   :: !Role
    , rEnv    :: !Env
    , rTrust  :: !TrustPath
    , rPolicy :: !PolicyPath
    } deriving (Show)

profileParser :: Parser Profile
profileParser = Profile
    <$> roleOption
    <*> envOption
    <*> trustOption
    <*> policyOption

instance Options Profile where
    discover _ Common{..} r@Profile{..} = return $! r
        { rTrust  = pTrustPath
        , rPolicy = pPolicyPath
        }
      where
        Policy{..} = Profile.policy r cConfig rTrust rPolicy

    validate Profile{..} = do
        checkPath (_trust  rTrust)  " specified by --trust must exist."
        checkPath (_policy rPolicy) " specified by --policy must exist."

instance Naming Profile where
    names Profile{..} = unversioned rRole rEnv

commands :: Mod CommandFields Command
commands = group "profile" "Long description." $ mconcat
    [ command "info" info profileParser
        "Create or update IAM profiles."
    , command "update" update profileParser
        "Create or update IAM profiles."
    ]

info :: Common -> Profile -> AWS ()
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
            . fmap dec
            $ rAssumeRolePolicyDocument p)
        ]
  where
    dec :: Text -> Maybe Object
    dec = decode . LBS.fromStrict . urlDecode True . Text.encodeUtf8

update :: Common -> Profile -> AWS ()
update _ r = void $ Profile.update r (rTrust r) (rPolicy r)
