{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}

-- Module      : Khan.CLI.Host
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Host (commands) where

import qualified Data.Text.Encoding       as Text
import qualified Khan.AWS.Route53         as R53
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2.Metadata
import           Network.AWS.Route53
import           Pipes
import qualified Pipes.Prelude            as Pipes

defineOptions "Host" $ do
    textOption "hId" "id" ""
        "Instance Id."

    textOption "hFQDN" "fqdn" ""
        "FQDN."

    integerOption "hTTL" "ttl" 120
        "TTL."

deriving instance Show Host

instance Discover Host where
    discover False h = return h
    discover True  h = liftEitherT $ do
        iid  <- Text.decodeUtf8 <$> metadata InstanceId
        fqdn <- Text.decodeUtf8 <$> metadata PublicHostname
        return $! h { hId = iid, hFQDN = fqdn }

instance Validate Host where
    validate Host{..} = do
        check hId   "--id must be specified."
        check hFQDN "--fqdn must be specified."

        check (not $ hTTL >= 30) "--ttl must be greater than or equal to 30."

commands :: [Command]
commands =
    [ command register "register" "Register an instance with DNS."
        "blah."
    , command deregister "deregister" "Deregister an instance from DNS."
        "blah."
    ]

-- FIXME: Handle errors retries gracefully
register :: Host -> AWS ()
register Host{..} = do
    log "Registering host {}..." [hFQDN]
    (ns@Names{..}, ts@Tags{..}) <- describe hId
    zid  <- R53.findZoneId tagDomain
    sets <- findPrefix zid roleName envName
    maybe (create zid ns ts sets) exists $ filterValue hFQDN sets
  where
    exists r = log "Record {} exists, skipping..." [rrsName r]

    create zid ns ts sets = do
        reg <- getRegion
        let n   = 1 + foldl' newest 0 sets
            dns = name ns ts reg n
            vs  = ResourceRecords [hFQDN]
            set = BasicRecordSet dns CNAME hTTL vs Nothing
        log "Creating record {} with value {}..." [dns, hFQDN]
        R53.updateRecordSet zid [Change CreateAction set]

    newest acc x = max (either (const 0) dnsOrd . parseDNS $ rrsName x) acc

    name Names{..} Tags{..} reg n =
        showDNS (DNS roleName tagVersion n envName $ R53.abbreviate reg) tagDomain

deregister :: Host -> AWS ()
deregister Host{..} = return ()
    -- (ns, Tags{..}) <- describe hId
    -- zid            <- R53.findZoneId tagDomain
    -- case tagVersion of
    --     Nothing -> persistent ns zid
    --     Just _  -> error_ "Not implemented"
--  where
    -- persistent Names{..} zid = do
    --     log "Searching for records matching {} with value {}..."
    --         [roleName, hFQDN]
    --     mset <- filterValue hFQDN <$> findPrefix zid roleName
    --     maybe (log_ "No record found, skipping...") delete
    --           mset

    -- delete set = do
    --     log_ "Deleting record..."
    --     R53.updateRecordSet zid [Change DeleteAction set]

describe :: Text -> AWS (Names, Tags)
describe iid = do
    t@Tags{..} <- requiredTags iid
    return . (,t) $
        maybe (unversioned tagRole tagEnv) (versioned tagRole tagEnv) tagVersion

findPrefix :: HostedZoneId -> Text -> Text -> AWS [ResourceRecordSet]
findPrefix zid pre env = Pipes.toListM $ paginate start
    >-> Pipes.map lrrsrResourceRecordSets
    >-> Pipes.concat
    >-> Pipes.filter (either (const False) match . parseDNS . rrsName)
  where
    start = ListResourceRecordSets zid Nothing Nothing Nothing Nothing

    match DNS{..} = dnsRole == pre && dnsEnv == env

filterValue :: Text -> [ResourceRecordSet] -> Maybe ResourceRecordSet
filterValue v sets = match `find` sets
  where
    match BasicRecordSet{..} = v `elem` rrValues rrsResourceRecords
    match _                  = False
