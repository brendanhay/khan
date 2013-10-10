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

import Data.Foldable (foldl')
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.List                (nub, intercalate)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Data.Text.Format
import qualified Khan.AWS.AutoScaling     as ASG
import qualified Khan.AWS.EC2             as EC2
import qualified Khan.AWS.IAM             as IAM
import qualified Khan.AWS.Route53         as R53
import           Khan.Internal
import           Network.AWS
import           Network.AWS.EC2
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
    ts   <- fmap toMap . send $ DescribeTags [TagResourceId [hId]]
    role <- required roleTag ts
    env  <- required envTag ts
    dom  <- required domainTag ts
    zid  <- R53.findZoneId dom

    let ver   = join $ parse <$> Map.lookup versionTag ts
        disco = Map.lookup discoTag ts
        ns    = maybe (unversioned role env) (versioned role env) ver

    maybe (persistent ns zid dom) (ephemeral ns zid) disco
  where
    toMap = Map.fromList
        . map (\TagSetItemType{..} -> (tsitKey, tsitValue))
        . dtagsrTagSet

    required k m = noteError (message k m) $ Map.lookup k m

    message k m = Text.unpack $
        Text.concat ["No tag '", k, "' found in [", render m, "]"]

    render = Text.intercalate ","
        . map (\(k, v) -> Text.concat [k, "=", v])
        . Map.toList

    parse = hush . parseVersionE . Text.unpack

    -- FIXME: Create and assign health check
    -- FIXME: Handle errors retries gracefully
    persistent Names{..} zid dom = do
        sets <- matching appName
        unless (any (exists hFQDN) sets) $ create sets
      where
        matching name = Pipes.toListM $ paginate start
            >-> Pipes.map lrrsrResourceRecordSets
            >-> Pipes.concat
            >-> Pipes.filter ((name ==) . Text.takeWhile (/= '.') . rrsName)

        start = ListResourceRecordSets zid Nothing Nothing Nothing Nothing

        exists v LatencyRecordSet{..} = any (== v) $ rrValues rrsResourceRecords
        exists _ _                    = False

        create sets = do
            reg <- currentRegion
            let n     = Text.pack . show $ 1 + foldl' newest 0 sets
                name  = Text.concat [appName, n, ".", R53.abbreviate reg]
                value = ResourceRecords [hFQDN]
                set   = LatencyRecordSet name CNAME appName reg 60 value Nothing
            R53.updateRecordSet zid [Change CreateAction set]

        newest acc x = max (int $ rrsName x) acc

        int = fromMaybe (0 :: Integer)
            . readMay
            . (:[])
            . Text.last
            . Text.takeWhile (/= '.')

    -- FIXME: Create and assign health check
    -- FIXME: Handle errors retries gracefully
    ephemeral Names{..} zid dns = do
        mset <- R53.findRecordSet zid ((dns ==) . rrsName)
        let value = Text.intercalate " " ["50", "50", "8080", hFQDN]
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

unregister :: Host -> AWS ()
unregister Host{..} = return ()
