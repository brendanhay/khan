{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Model.IAM.ServerCertificate
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.IAM.ServerCertificate
    ( find
    , upload
    , delete
    ) where

import qualified Filesystem      as FS
import           Khan.Internal
import           Khan.Prelude    hiding (find)
import           Network.AWS.IAM

find :: Text -> AWS (Maybe ServerCertificateMetadata)
find dom = do
    say "Searching for Certificate {}" [dom]
    sendCatch (GetServerCertificate dom) >>= verify
  where
    verify (Right x) = return $ Just (unwrap x)
    verify (Left  e)
        | "NoSuchEntity" == etCode (erError e) = return Nothing
        | otherwise = throwError (toError e)

    unwrap = scServerCertificateMetadata
        . gscrServerCertificate
        . gscrGetServerCertificateResult

upload :: Text -> FilePath -> FilePath -> Maybe FilePath -> AWS ()
upload dom pubp privp chainp = do
    (pub, priv, chain) <- liftAWS $
        (,,) <$> loadKey pubp "Reading public key from {}"
             <*> loadKey privp "Reading private key from {}"
             <*> loadChain chainp

    say "Upload Certificate {}" [dom]
    send_ UploadServerCertificate
        { uscCertificateBody       = pub
        , uscPrivateKey            = priv
        , uscCertificateChain      = chain
        , uscPath                  = Nothing
        , usdServerCertificateName = dom
        }

    say "Created Certificate {}" [dom]
  where
    loadKey path fmt = FS.readTextFile path <* say fmt [path]

    loadChain (Just p) = Just <$> loadKey p "Reading certificate chain from {}"
    loadChain Nothing  = return Nothing

delete :: Text -> AWS ()
delete dom = do
    say "Deleting Certificate {}" [dom]
    send_ (DeleteServerCertificate dom)
