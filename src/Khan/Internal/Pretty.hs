{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
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
    ( ppHead
    , ppBody
    , ppGroup
    , ppLine
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

newtype Head a = Head { unHead :: a }
newtype Body a = Body a

data Column where
    H :: Pretty a => a -> Column
    C :: Pretty a => a -> Column
    W :: Int -> Column -> Column

instance Pretty Column where
    pretty (H   x) = bold (pretty x)
    pretty (C   x) = pretty x
    pretty (W n x) = fill n (pretty x)

ppHead :: (MonadIO m, Pretty (Head a)) => a -> m ()
ppHead = ppLog bold . Head

ppBody :: (MonadIO m, Pretty (Body a)) => a -> m ()
ppBody = ppLog id . Body

ppGroup :: (MonadIO m, Pretty (Head a), Pretty (Body a)) => a -> m ()
ppGroup x = ppHead x >> ppBody x >> ppLine

ppLine :: MonadIO m => m ()
ppLine = ppLog id ""

ppLog :: (MonadIO m, Pretty a) => (Doc -> Doc) -> a -> m ()
ppLog f = log "{}" . Only . ($ "") . displayS . renderPretty 0.4 100 . f . pretty

title :: Pretty a => a -> Doc
title n = ("" <->) . after $
    lbracket <> bold (green $ pretty n) <> rbracket <+> "->"

after :: Doc -> Doc
after = (<-> "")

cols :: Int -> [Column] -> [Doc]
cols w = map f
  where
    f x@W{} = pretty x
    f x     = fill w (pretty x)

hcols :: Int -> [Column] -> Doc
hcols w = indent 2 . hsep . cols w

vrow :: Pretty a => Int -> a -> Doc
vrow w = indent 2 . fill w . bold . pretty

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

instance Pretty (Head IAM.Role) where
    pretty = title . IAM.rRoleName . unHead

instance Pretty (Body IAM.Role) where
    pretty (Body IAM.Role{..}) = vsep hs <-> policy
      where
        w  = vrow 23

        hs = [ w "arn:"         <+> pretty rArn
             , w "role-id:"     <+> pretty rRoleId
             , w "path:"        <+> pretty rPath
             , w "create-date:" <+> pretty rCreateDate
             ]

        policy = w "assume-policy-document:" <+>
            (pretty . prettyJSON $ decodeURL <$> rAssumeRolePolicyDocument)

instance Pretty (Body IAM.GetRolePolicyResult) where
    pretty (Body IAM.GetRolePolicyResult{..}) = vrow 23 "policy-document:" <+>
        (pretty . prettyJSON $ decodeURL grprPolicyDocument)

instance Pretty (Head (Ann ASG.AutoScalingGroup)) where
    pretty (Head (Ann ASG.AutoScalingGroup{..} Tags{..})) =
        title asgAutoScalingGroupName <-> hcols 10
            [ W 8 (H "status:")
            , W 8 (H "weight:")
            , H "version:"
            , W 19 (H "zones:")
            , H "cooldown:"
            , H "[m..N]:"
            , H "desired:"
            , W 19 (H "created:")
            ]

instance Pretty (Body (Ann ASG.AutoScalingGroup)) where
    pretty (Body (Ann ASG.AutoScalingGroup{..} Tags{..})) = hcols 10
        [ W 8 (C $ maybe "OK" pretty asgStatus)
        , W 8 (C tagWeight)
        , C tagVersion
        , W 19 (C asgAvailabilityZones)
        , C asgDefaultCooldown
        , C (pretty asgMinSize <> ".." <> pretty asgMaxSize)
        , C asgDesiredCapacity
        , W 19 (C asgCreatedTime)
        ]

instance Pretty ASG.Filter where
    pretty ASG.Filter{..} = pretty fName <> char ':' <> pretty fValues

instance Pretty AvailabilityZone where
    pretty AZ{..} = pretty azRegion <> char azSuffix

    prettyList []     = mempty
    prettyList [a]    = pretty a
    prettyList (a:as) = pretty (azRegion a) <> char ':' <+>
        hcat (punctuate comma $ map (char . azSuffix) as)

instance Pretty (Head (Ann EC2.RunningInstancesItemType)) where
    pretty (Head _) = hcols 15
        [ W 8 (H "state:")
        , W 8 (H "weight:")
        , H "instance-id:"
        , H "image-id:"
        , H "public-ip:"
        , H "type:"
        , W 19 (H "launched:")
        ]

instance Pretty (Body (Ann EC2.RunningInstancesItemType)) where
    pretty (Body (Ann EC2.RunningInstancesItemType{..} Tags{..})) = hcols 15
        [ W 8 (C $ EC2.istName riitInstanceState)
        , W 8 (C tagWeight)
        , C riitInstanceId
        , C riitImageId
        , C (fromMaybe "" riitIpAddress)
        , C riitInstanceType
        , W 19 (C riitLaunchTime)
        ]

instance Pretty EC2.InstanceType where
    pretty = fromString . show

instance Pretty (Head EC2.SecurityGroupItemType) where
    pretty = title . EC2.sgitGroupName . unHead

instance Pretty (Body EC2.SecurityGroupItemType) where
    pretty (Body EC2.SecurityGroupItemType{..}) = vsep
        [ w "owner-id:"              <+> pretty sgitOwnerId
        , w "group-id:"              <+> pretty sgitGroupId
        , w "group-description:"     <+> pretty sgitGroupDescription
        , w "vpc-id:"                <+> pretty (fromMaybe "<blank>" sgitVpcId)
        , w "ip-permissions-egress:" <+> pretty sgitIpPermissionsEgress
        , w "ip-permissions:"        <+> pretty sgitIpPermissions
        ]
      where
        w = vrow 24

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

instance Pretty (Head R53.HostedZone) where
    pretty (Head R53.HostedZone{..}) = title hzName <-> hcols 10
        [ W 38 (H "id:")
        , W 38 (H "reference:")
        , H "config:"
        , H "record-count:"
        ]

instance Pretty (Body R53.HostedZone) where
    pretty (Body R53.HostedZone{..}) = hcols 10
        [ W 38 (C hzId)
        , W 38 (C hzCallerReference)
        , C hzConfig
        , C hzResourceRecordSetCount
        ]

instance Pretty (Body R53.ResourceRecordSet) where
    pretty (Body x) = after $
        hcols w (heading ++ hs x) <-> hcols w (common ++ bs x)
      where
        w = 10

        wide = W 36

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
 
