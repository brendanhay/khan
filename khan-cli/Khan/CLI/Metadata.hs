{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Khan.CLI.Metadata (commands) where

-- Module      : Khan.CLI.Metadata
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

import qualified Data.Aeson                       as Aeson
import           Data.Aeson                       (Value(String))
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.HashMap.Strict              as Map
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.Lazy.Builder           as Build
import qualified Data.Text.Lazy.IO                as LText
import           Khan.Internal
import qualified Khan.Model.Ansible.Serialisation as Ansible
import qualified Khan.Model.Tag                   as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata         (Dynamic(..), toPath)
import qualified Network.AWS.EC2.Metadata         as Meta
import qualified Text.EDE.Filters                 as EDE

data Describe = Describe
    { dMultiLine :: !Bool
    }

describeParser :: Parser Describe
describeParser = Describe
    <$> switchOption "multiline" False
        "Write each output KEY=VALUE on a separate line."

instance Options Describe where
    discover False _ _ =
        throwError "khan metadata can only be run on an EC2 instance."
    discover True  _ d =
        return d

commands :: Mod CommandFields Command
commands = command "metadata" describe describeParser
    "Collect and display various metadata about the running instance."

describe :: Common -> Describe -> AWS ()
describe Common{..} Describe{..} = do
    bs  <- liftEitherT $ Meta.dynamic Document
    debug "Received dynamic document:\n{}" [Text.decodeUtf8 bs]

    doc <- filtered <$> decode bs
    iid <- instanceId doc
    ts  <- Tag.require iid

    liftIO . LText.putStrLn
           . Build.toLazyText
           . renderEnv dMultiLine
           $ toEnv ts <> hostVars ts <> doc
  where
    decode = noteError "Unable to decode: "
        . Aeson.decode
        . LBS.fromStrict

    filtered m =
        let key = awsPrefix . Text.toUpper . EDE.underscore
         in Map.fromList [(key k, v) | (k, Just v) <- Map.toList m]

    instanceId = lookupKey (awsPrefix "INSTANCE_ID")
    awsPrefix  = mappend "AWS_"

    lookupKey k = noteError ("Unable to find " <> k <> " in: ") . Map.lookup k

    noteError e = hoistError
        . note (toError . Text.unpack $ e
               <> "http://169.254.169.254/latest/dynamic/"
               <> toPath Document)

    hostVars =
        let f (k, String v) = Just (Text.toUpper k, v)
            f _             = Nothing
         in Map.fromList . mapMaybe f . Ansible.hostVars cRegion . names
