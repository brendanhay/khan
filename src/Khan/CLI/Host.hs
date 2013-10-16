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

module Khan.CLI.Host (cli) where

import           Data.Char                (isDigit)
import           Data.List                (nub)
import qualified Data.Text                as Text
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

deriving instance Show Host

instance Discover Host where
    discover h = liftEitherT $ do
        iid  <- Text.decodeUtf8 <$> metadata InstanceId
        fqdn <- Text.decodeUtf8 <$> metadata PublicHostname
        return $! h { hId = iid, hFQDN = fqdn }

instance Validate Host where
    validate Host{..} = do
        check hId   "--id must be specified."
        check hFQDN "--fqdn must be specified."

cli :: Command
cli = Command "host" "Manage EC2 Hosts."
    [ subCommand "register"   register
    , subCommand "deregister" deregister
    ]

register :: Host -> AWS ()
register Host{..} = do
    (ns, Tags{..}) <- describe hId
    zid            <- R53.findZoneId tagDomain
    maybe (persistent ns zid tagDomain) (ephemeral ns zid tagDomain) tagVersion
  where
    -- FIXME: Create and assign health check
    -- FIXME: Handle errors retries gracefully
    persistent Names{..} zid dom = do
        logInfo_ "Registering as persistent host..."
        sets <- findPrefix zid roleName
        maybe (create sets) exists $ filterValue hFQDN sets
      where
        create sets = do
            reg <- currentRegion
            let n    = 1 + foldl' newest 0 sets
                name = address roleName (Text.pack $ show n) envName reg dom
                vs   = ResourceRecords [hFQDN]
                set  = BasicRecordSet name CNAME 60 vs Nothing
            logInfo "Creating record {} with value {}..." [name, hFQDN]
            R53.updateRecordSet zid [Change CreateAction set]

        exists r = logInfo "Record {} exists, skipping..." [rrsName r]

        newest acc x = max (int $ rrsName x) acc

        int = fromMaybe (0 :: Integer)
            . readMay
            . (:[])
            . Text.last
            . Text.takeWhile (/= '.')

    -- FIXME: Create and assign health check
    -- FIXME: Handle errors retries gracefully
    ephemeral Names{..} zid dom (safeVersion -> ver) = do
        reg <- currentRegion
        let name  = address roleName ver envName reg dom
            value = Text.intercalate " " ["50", "50", "8080", hFQDN]
        logInfo "Registering {} with {} as ephemeral host..." [hFQDN, name]
        mset <- R53.findRecordSet zid ((name ==) . rrsName)
        R53.updateRecordSet zid $ maybe (create name value) (update value) mset
      where
        create n v =
            [ Change CreateAction $
                BasicRecordSet n SRV 60 (ResourceRecords [v]) Nothing
            ]

        update v s =
            let vs = ResourceRecords . nub $ v : rrValues (rrsResourceRecords s)
            in [ Change DeleteAction s
               , Change CreateAction $ s { rrsResourceRecords = vs }
               ]

deregister :: Host -> AWS ()
deregister Host{..} = do
    (ns, Tags{..}) <- describe hId
    zid            <- R53.findZoneId tagDomain
    maybe (persistent ns zid) (error . show) tagVersion
  where
    persistent Names{..} zid = do
        logInfo "Searching for records matching {} with value {}..."
            [roleName, hFQDN]
        mset <- filterValue hFQDN <$> findPrefix zid roleName
        maybe (logInfo_ "No record found, skipping...") delete
              mset
      where
        delete set = do
            logInfo_ "Deleting record..."
            R53.updateRecordSet zid [Change DeleteAction set]

describe :: Text -> AWS (Names, Tags)
describe iid = do
    t@Tags{..} <- requiredTags iid
    return . (,t) $
        maybe (unversioned tagRole tagEnv) (versioned tagRole tagEnv) tagVersion

findPrefix :: HostedZoneId -> Text -> AWS [ResourceRecordSet]
findPrefix zid k = Pipes.toListM $ paginate start
    >-> Pipes.map lrrsrResourceRecordSets
    >-> Pipes.concat
    >-> Pipes.filter ((k ==) . Text.takeWhile (not . isDigit) . rrsName)
  where
    start = ListResourceRecordSets zid Nothing Nothing Nothing Nothing

filterValue :: Text -> [ResourceRecordSet] -> Maybe ResourceRecordSet
filterValue v sets = match `find` sets
  where
    match BasicRecordSet{..} = any (== v) $ rrValues rrsResourceRecords
    match _                  = False

address :: Text -> Text -> Text -> Region -> Text -> Text
address role ver env reg dom = Text.intercalate "."
    [ role <> ver
    , R53.abbreviate reg
    , env
    , dom
    ]
