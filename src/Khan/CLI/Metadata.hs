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

import qualified Data.Aeson               as Aeson
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.HashMap.Strict      as Map
import qualified Data.Text                as Text
import qualified Data.Text.Lazy.Builder   as Build
import qualified Data.Text.Lazy.IO        as LText
import           Khan.Internal
import qualified Khan.Model.Tag           as Tag
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata (Dynamic(..), toPath)
import qualified Network.AWS.EC2.Metadata as Meta

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
describe _ Describe{..} = do
    doc <- liftEitherT (Meta.dynamic Document) >>= decode
    iid <- instanceId doc
    ts  <- Tag.required iid

    liftIO
        . LText.putStrLn
        . Build.toLazyText
        . renderEnv dMultiLine
        $ toEnv ts <> doc
  where
    decode = noteError "Unable to decode: "
        . Aeson.decode
        . LBS.fromStrict

    instanceId = noteError "Unable to find INSTANCE_ID in: "
        . Map.lookup "INSTANCE_ID"

    noteError m = hoistError
        . note (toError . Text.unpack $ m
               <> "http://169.254.169.254/latest/dynamic-data/"
               <> toPath Document)
