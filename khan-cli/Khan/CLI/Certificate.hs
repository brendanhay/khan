{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Khan.CLI.Certificate
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Certificate (commands) where

import           Khan.Internal
import qualified Khan.Model.IAM.ServerCertificate as Cert
import           Khan.Prelude
import           Network.AWS.IAM

data Info = Info
    { iDomain :: !Text
    } deriving (Show)

infoParser :: Parser Info
infoParser = Info
    <$> textOption "domain" (short 'd')
        "Certificates's domain name."

instance Options Info where
    validate Info{..} =
        check iDomain "--domain must be specified."

data Upload = Upload
    { uDomain :: !Text
    , uPublic  :: !FilePath
    , uPrivate :: !FilePath
    , uChain   :: Maybe FilePath
    } deriving (Show)

uploadParser :: Parser Upload
uploadParser = Upload
    <$> textOption "domain" (short 'd')
        "The certificates's domain name."
    <*> pathOption "public" (action "file")
        "The contents of the public key in PEM-encoded format."
    <*> pathOption "private" (action "file")
        "The contents of the private key in PEM-encoded format."
    <*> optional (pathOption "chain" (action "file")
        "A concatenation of PEM-encoded public keys.")

instance Options Upload where
    validate Upload{..} = do
        check uDomain "--domain must be specified."

        checkPath uPublic  " specified by --public must exist."
        checkPath uPrivate " specified by --private must exist."

        maybe (return ())
              (`checkPath` " specified by --chain must exist.")
              uChain

data Delete = Delete
    { dDomain :: !Text
    } deriving (Show)

deleteParser :: Parser Delete
deleteParser = Delete
    <$> textOption "domain" (short 'd')
        "Certificates's domain name."

instance Options Delete where
    validate Delete{..} =
        check dDomain "--domain must be specified."

commands :: Mod CommandFields Command
commands = group "certificate" "IAM Server Certificates." $ mconcat
    [ command "info" info infoParser
        "Display information about an IAM Server Certificate."
    , command "upload" upload uploadParser
        "Upload an IAM Server Certificate."
    , command "delete" delete deleteParser
        "Delete an IAM Server Certificate."
    ]

info :: Common -> Info -> AWS ()
info _ Info{..} = Cert.find iDomain >>=
    maybe (log_ "Unable to find Certificate.")
          (pPrint . overview)

upload :: Common -> Upload -> AWS ()
upload _ Upload{..} = Cert.find uDomain >>=
    maybe (Cert.upload uDomain uPublic uPrivate uChain)
          (const $ throwAWS "Certificate already exists: {}" [uDomain])

delete :: Common -> Delete -> AWS ()
delete _ Delete{..} = Cert.delete dDomain
