{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Khan.Internal.Pretty
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Internal.Pretty
    ( Pretty (..)
    , renderCompact

    , Ann    (..)
    , pp
    , ppi
    , ln
    , title
    ) where

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.List                    (sort)
import           Data.SemVer
import           Data.String
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Text.Format             (Only(..))
import           Data.Time                    (UTCTime)
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.AutoScaling      as ASG
import qualified Network.AWS.EC2              as EC2
import qualified Network.AWS.IAM              as IAM
import qualified Network.AWS.Route53          as R53
import           Network.HTTP.Types           (urlDecode)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

default (Doc)

data Column where
    H :: Pretty a => a -> Column
    C :: Pretty a => a -> Column
    D :: Pretty a => Int -> a -> Column

instance Pretty Column where
    pretty (H   x) = bold (pretty x)
    pretty (C   x) = pretty x
    pretty (D n x) = fill n (pretty x)

cols :: Int -> [Column] -> [Doc]
cols w = map f
  where
    f (H   x) = fill w (bold $ pretty x)
    f (C   x) = fill w (pretty x)
    f (D n x) = fill n (pretty x)

hcols :: Int -> [Column] -> Doc
hcols w = hsep . cols w

vrow :: Pretty a => Int -> a -> Doc
vrow w = fill w . bold . pretty

title :: Pretty a => a -> Doc
title n = lbracket <> bold (green $ pretty n) <> rbracket <+> "->"

pp :: (MonadIO m, Pretty a) => a -> m ()
pp = ppi 0

ppi :: (MonadIO m, Pretty a) => Int -> a -> m ()
ppi i = log "{}" . Only . ($ "") . displayS . renderPretty 0.4 100 . indent i . pretty

ln :: MonadIO m => m ()
ln = liftIO (putStrLn "")

(<->) :: Doc -> Doc -> Doc
(<->) = (PP.<$>)

prettyJSON :: ToJSON a => Maybe a -> Text
prettyJSON = Text.decodeUtf8 . LBS.toStrict . maybe "" Aeson.encodePretty

decodeURL :: Text -> Maybe Object
decodeURL = decode . LBS.fromStrict . urlDecode True . Text.encodeUtf8

instance Pretty Text where
    pretty = text . Text.unpack

instance Pretty UTCTime where
    pretty = text . formatUTC

instance Pretty Region where
    pretty = text . show

instance Pretty FilePath where
    pretty = text . Path.encodeString

instance Pretty Version where
    pretty = pretty . showVersion

instance Pretty IAM.Role where
    pretty IAM.Role{..} = vsep hs <-> policy
      where
        w  = 23

        hs = [ vrow w "arn:"         <+> pretty rArn
             , vrow w "role-id:"     <+> pretty rRoleId
             , vrow w "path:"        <+> pretty rPath
             , vrow w "create-date:" <+> pretty rCreateDate
             ]

        policy = vrow w "assume-policy-document:" <+>
            (pretty . prettyJSON $ decodeURL <$> rAssumeRolePolicyDocument)

instance Pretty IAM.GetRolePolicyResult where
    pretty IAM.GetRolePolicyResult{..} = fill 23 (bold "policy-document:") <+>
            (pretty . prettyJSON $ decodeURL grprPolicyDocument)

instance Pretty (Ann ASG.AutoScalingGroup) where
    pretty (Ann ASG.AutoScalingGroup{..} Tags{..}) =
        hcols w hs <-> hcols w bs
      where
        w  = 10
        z  = 8
        d  = 19

        hs = [ H "status:"
             , H (D z "weight:")
             , H "version:"
             , H (D d "zones:")
             , H "cooldown:"
             , H "[m..N]:"
             , H "desired:"
             , H (D d "created:")
             ]

        bs = [ C (maybe "OK" pretty asgStatus)
             , C (D z tagWeight)
             , C tagVersion
             , C (D d asgAvailabilityZones)
             , C asgDefaultCooldown
             , C (pretty asgMinSize <> ".." <> pretty asgMaxSize)
             , C asgDesiredCapacity
             , C (D d asgCreatedTime)
             ]

instance Pretty ASG.Filter where
    pretty ASG.Filter{..} = pretty fName <> char ':' <> pretty fValues

instance Pretty AvailabilityZone where
    pretty AZ{..} = pretty azRegion <> char azSuffix

    prettyList []     = mempty
    prettyList [a]    = pretty a
    prettyList (a:as) = pretty (azRegion a) <> char ':' <+>
        hcat (punctuate comma $ map (char . azSuffix) as)

instance Pretty (Ann EC2.RunningInstancesItemType) where
    pretty (Ann EC2.RunningInstancesItemType{..} Tags{..}) = hcols 15
        [ C (EC2.istName riitInstanceState)
        , C (D 9 tagWeight)
        , C riitInstanceId
        , C riitImageId
        , C (fromMaybe "" riitIpAddress)
        , C riitInstanceType
        , C (D 19 riitLaunchTime)
        ]

    prettyList is = hcols 15 hs <-> vsep (map pretty is)
      where
        hs = [ H "state:"
             , H (D 9 "weight:")
             , H "instance-id:"
             , H "image-id:"
             , H "public-ip:"
             , H "type:"
             , H (D 19 "launched:")
             ]

instance Pretty EC2.InstanceType where
    pretty = fromString . show

instance Pretty EC2.SecurityGroupItemType where
    pretty EC2.SecurityGroupItemType{..} = vsep
        [ vrow w "owner-id:"              <+> pretty sgitOwnerId
        , vrow w "group-id:"              <+> pretty sgitGroupId
        , vrow w "group-description:"     <+> pretty sgitGroupDescription
        , vrow w "vpc-id:"                <+> pretty (fromMaybe "<blank>" sgitVpcId)
        , vrow w "ip-permissions-egress:" <+> pretty sgitIpPermissionsEgress
        , vrow w "ip-permissions:"        <+> pretty sgitIpPermissions
        ]
      where
        w = 24

instance Pretty EC2.IpPermissionType where
    pretty EC2.IpPermissionType{..} = hcat
        [ pretty iptIpProtocol
        , char ':'
        , pretty iptToPort
        , char ':'
        , pretty iptToPort
        , char ':'
        , tupled . map pretty . sort $ ranges ++ groups
        ]
      where
        ranges = map EC2.irCidrIp $ toList iptIpRanges
        groups = mapMaybe EC2.uigGroupName $ toList iptGroups

instance Pretty EC2.Protocol where
    pretty = text . show

instance Pretty R53.CallerReference where
    pretty = pretty . R53.unCallerReference

instance Pretty R53.HostedZoneId where
    pretty = pretty . R53.unHostedZoneId

instance Pretty R53.HealthCheckId where
    pretty = pretty . R53.unHealthCheckId

instance Pretty R53.RecordType where
    pretty = text . show

instance Pretty R53.AliasTarget where
    pretty R53.AliasTarget{..} = hcat
        [ pretty atHostedZoneId
        , "->"
        , pretty atDNSName
        , " health:"
        , bool atEvaluateTargetHealth
        ]

instance Pretty R53.ResourceRecords where
    pretty = pretty . R53.rrValues

instance Pretty R53.Config where
    pretty = pretty . fromMaybe "<blank>" . R53.cComment

instance Pretty R53.Failover where
    pretty = text . show

instance Pretty R53.HostedZone where
    pretty R53.HostedZone{..} = hcols w hs <-> hcols w bs
      where
        w = 10

        wide = D 38

        hs = [ wide (H "id:")
             , wide (H "reference:")
             , H "config:"
             , H "record-count:"
             ]

        bs = [ wide (C hzId)
             , wide (C hzCallerReference)
             , C hzConfig
             , C hzResourceRecordSetCount
             ]

instance Pretty R53.ResourceRecordSet where
    pretty x = hcols w (heading ++ hs x) <-> hcols w (common ++ bs x)
      where
        w = 10

        wide = D 36

        heading =
            [ wide (H "name:")
            , H "type:"
            ]

        common =
            [ wide (C . R . B $ R53.rrsName x)
            , C (R53.rrsType x)
            ]

        hs R53.FailoverRecordSet{} =
            [ H "failover:"
            , H "ttl:"
            , wide (H "set-id:")
            , wide (H "values:")
            ]

        hs R53.FailoverAliasRecordSet{} =
            [ H "failover:"
            , wide (H "set-id:")
            , H "alias-target:"
            ]

        hs R53.LatencyRecordSet{} =
            [ H "ttl:"
            , H "region:"
            , wide (H "set-id:")
            , wide (H "values:")
            ]

        hs R53.LatencyAliasRecordSet{} =
            [ H "region:"
            , wide (H "set-id:")
            , H "alias-target:"
            ]

        hs R53.WeightedRecordSet{} =
            [ H "ttl:"
            , H "weight:"
            , wide (H "set-id:")
            ]

        hs R53.WeightedAliasRecordSet{} =
            [ H "weight:"
            , wide (H "set-id:")
            , H "alias-target:"
            ]

        hs R53.BasicRecordSet{} =
            [ H "ttl:"
            , wide (H "values:")
            ]

        hs R53.AliasRecordSet{} =
            [ H "alias-target:"
            ]

        bs R53.FailoverRecordSet{..} =
            [ C rrsFailover
            , C rrsTTL
            , setId
            , values
            ]

        bs R53.FailoverAliasRecordSet{..} =
            [ C rrsFailover
            , setId
            , alias
            ]

        bs R53.LatencyRecordSet{..} =
            [ C rrsTTL
            , C rrsRegion
            , setId
            , values
            ]

        bs R53.LatencyAliasRecordSet{..} =
            [ C rrsRegion
            , setId
            , alias
            ]

        bs R53.WeightedRecordSet{..} =
            [ C rrsTTL
            , C rrsWeight
            , setId
            ]

        bs R53.WeightedAliasRecordSet{..} =
            [ C rrsWeight
            , setId
            , alias
            ]

        bs R53.BasicRecordSet{..} =
            [ C rrsTTL
            , values
            ]

        bs R53.AliasRecordSet{..} =
            [ alias
            ]

        setId  = wide $ C (R53.rrsSetIdentifier x)
        values = wide $ C (R53.rrsResourceRecords x)
        alias  = wide $ C (R53.rrsAliasTarget x)
