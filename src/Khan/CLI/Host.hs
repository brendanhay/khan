{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

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

import           Control.Applicative
import           Control.Error
import           Data.Char                (isDigit)
import           Data.Foldable            (foldl')
import           Data.List                (find, nub)
import           Data.Monoid
import qualified Data.Text                as Text
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as Text
import qualified Khan.AWS.Route53         as R53
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2.Metadata
import           Network.AWS.Route53
import           Pipes
import qualified Pipes.Prelude            as Pipes

defineOptions "Host" $ do
    textOption "hId" "id" ""
        "Name of the group."

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
    , subCommand "unregister" unregister
    ]

register :: Host -> AWS ()
register Host{..} = do
    Tags{..} <- requiredTags hId
    zid      <- R53.findZoneId tagDomain
    let ns = maybe (unversioned tagRole tagEnv) (versioned tagRole tagEnv)
                   tagVersion
    maybe (persistent ns hFQDN zid tagDomain) (ephemeral ns hFQDN zid)
          tagDisco

unregister :: Host -> AWS ()
unregister Host{..} = return ()

-- FIXME: Create and assign health check
-- FIXME: Handle errors retries gracefully
persistent :: Names -> Text -> HostedZoneId -> Text -> AWS ()
persistent Names{..} fqdn zid dom = do
    sets <- matchPrefix roleName
    maybe (create sets) exists $ matchValue fqdn `find` sets
  where
    matchPrefix k = Pipes.toListM $ paginate start
        >-> Pipes.map lrrsrResourceRecordSets
        >-> Pipes.concat
        >-> Pipes.filter ((k ==) . Text.takeWhile (not . isDigit) . rrsName)

    start = ListResourceRecordSets zid Nothing Nothing Nothing Nothing

    matchValue v BasicRecordSet{..} =
        any (== v) $ rrValues rrsResourceRecords
    matchValue _ _                  = False

    create sets = do
        reg <- currentRegion
        let n    = Text.pack . show $ 1 + foldl' newest 0 sets
            name = Text.intercalate "."
                       [roleName <> n, envName, R53.abbreviate reg, dom]
            vs   = ResourceRecords [fqdn]
            set  = BasicRecordSet name CNAME 60 vs Nothing
        logInfo "Creating record {} with value {}..." [name, fqdn]
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
ephemeral :: Names -> Text -> HostedZoneId -> Text -> AWS ()
ephemeral Names{..} fqdn zid dns = do
    mset <- R53.findRecordSet zid ((dns ==) . rrsName)
    let value = Text.intercalate " " ["50", "50", "8080", fqdn]
    R53.updateRecordSet zid $ maybe (create value) (update value) mset
  where
    create value =
        [ Change CreateAction $
            BasicRecordSet appName SRV 60 (ResourceRecords [value]) Nothing
        ]

    update value s =
        let vs = ResourceRecords . nub $ value : rrValues (rrsResourceRecords s)
        in [ Change DeleteAction s
           , Change CreateAction $ s { rrsResourceRecords = vs }
           ]
