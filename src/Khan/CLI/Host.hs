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
import           Data.List                (delete)
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

defineOptions "Host" $
    textOption "iId" "id" ""
        "Name of the group."

deriving instance Show Host

instance Discover Host where
    discover d
        | not $ invalid (iId d) = return d
        | otherwise = liftEitherT $ do
            iid <- Text.decodeUtf8 <$> metadata InstanceId
            return $! d { iId = iid }

instance Validate Host

cli :: Command
cli = Command "instance" "Manage EC2 Hosts."
    [ subCommand "register"   register
    , subCommand "unregister" unregister
    ]

register :: Host -> AWS ()
register Host{..} = do
    ts   <- fmap toMap . send $ DescribeTags [TagResourceId [iId]]
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

    required k m = noteError
        ("Non-existent tag: " ++ Text.unpack k ++ " in: " ++ show m) $
            Map.lookup k m

    parse = hush . parseVersionE . Text.unpack

    -- FIXME: Create and assign health check
    -- FIXME: Handle errors retries gracefully
    persistent Names{..} zid dom = do
        fqdn <- Text.decodeUtf8 <$> liftEitherT (metadata PublicHostname)
        sets <- matching appName
        unless (any (exists fqdn) sets) $
            create fqdn sets
      where
        matching name = Pipes.toListM $ paginate start
            >-> Pipes.map lrrsrResourceRecordSets
            >-> Pipes.concat
            >-> Pipes.filter ((name ==) . Text.takeWhile (/= '.') . rrsName)

        start = ListResourceRecordSets zid Nothing Nothing Nothing Nothing

        exists v LatencyRecordSet{..} = any (== v) $ rrValues rrsResourceRecords
        exists _ _                    = False

        create fqdn sets = do
            reg <- currentRegion
            let n     = Text.pack . show $ 1 + foldl' newest 0 sets
                name  = Text.concat [appName, n, ".", R53.abbreviate reg]
                value = ResourceRecords [fqdn]
                set   = LatencyRecordSet name CNAME appName reg 60 value Nothing
            R53.updateRecordSet zid [Change CreateAction set]

        newest acc x = max (int $ rrsName x) acc

        int = fromMaybe (0 :: Integer)
            . readMay
            . (:[])
            . Text.last
            . Text.takeWhile (/= '.')

    ephemeral Names{..} zid dns = do
      --   mset <- R53.findRecordSet zid dns
      --   reg  <- currentRegion
      --   host <- Text.decodeUtf8 <$> liftEitherT (metadata PublicHostname)
      --   R53.findRecordSet zid appName >>= maybe (create host) (update host)
      -- where
      --   create host = do
      --       reg <- currentRegion
      --       -- create health check

      --       let vs  = ResourceRecords [host]
      --           set = LatencyRecordSet appName CNAME appName reg 60 vs Nothing

      --       R53.updateRecordSet zid [Change CreateAction set]

      --   update host rset = do
      --       let vs  = host : host `delete` rrValues (rrsResourceRecords rset)
      --           set = rset { rrsResourceRecords = ResourceRecords vs }

      --       R53.updateRecordSet zid
      --           [ Change DeleteAction rset
      --           , Change CreateAction set
      --           ]

        return ()

        -- Get record set or create if it doesn't exist
        -- Add self to srv

unregister :: Host -> AWS ()
unregister Host{..} = return ()
