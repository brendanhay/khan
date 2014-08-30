{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
    (
    -- * IO
      pPrint
    , pPrintLn
    , pLn

    -- * Documents
    , Column (..)
    , pretty
    , title
    , header
    , body
    , overview
    , line

    -- * Classes
    , Header
    , Body

    -- * Proxy
    , Proxy  (..)

    -- * Combinators
    , (<->)
    , indent
    , cols
    , hcols
    , vrow
    ) where

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.List                    (sort)
import           Data.Proxy                   (Proxy(..))
import           Data.SemVer                  (Version)
import qualified Data.SemVer                  as Ver
import           Data.String
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Text.Format             (Only(..))
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder       (Builder)
import qualified Data.Text.Lazy.Builder       as LText
import           Data.Time                    (UTCTime)
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Internal.Types
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.AutoScaling      as ASG
import qualified Network.AWS.EC2              as EC2
import qualified Network.AWS.ELB              as ELB
import qualified Network.AWS.IAM              as IAM
import qualified Network.AWS.Route53          as R53
import           Network.HTTP.Types           (urlDecode)
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), (<>))

default (Doc)

pPrint :: MonadIO m => Doc -> m ()
pPrint = log "{}"
     . Only
     . ($ "")
     . displayS
     . renderPretty 0.4 100

pPrintLn :: MonadIO m => Doc -> m ()
pPrintLn = pPrint . (<> line)

pLn :: MonadIO m => m ()
pLn = log_ ""

data Column where
    H :: Pretty a => a -> Column
    C :: Pretty a => a -> Column
    W :: Int -> Column -> Column
    Z :: Column

instance Pretty Column where
    pretty (H   x) = bold (pretty x)
    pretty (C   x) = pretty x
    pretty (W n x) = fill n (pretty x)
    pretty Z       = mempty

class Title a where
    title :: a -> Doc

instance Title a => Title (Ann a) where
    title = title . annValue

instance Title Text where
    title n = line <> lbracket <> bold (green $ pretty n) <> rbracket <+> "->"

class Header a where
    header :: Proxy a -> Doc

instance Header a => Header (Ann a) where
    header x = header (reproxy x :: Proxy a)

instance Header a => Header [a] where
    header _ = header (Proxy :: Proxy a)

class Body a where
    body :: a -> Doc

instance Body a => Body [a] where
    body = vsep . map body

overview :: forall a. (Title a, Header a, Body a) => a -> Doc
overview x = title x <-> header (Proxy :: Proxy a) <-> body x <> line

infixr 5 <->

(<->) :: Doc -> Doc -> Doc
a <-> b = a <> line <> b

cols :: Int -> [Column] -> [Doc]
cols w = map f
  where
    f x@W{} = pretty x
    f x     = fill w (pretty x)

hcols :: Int -> [Column] -> Doc
hcols w = indent 1 . hsep . cols w

vrow :: Pretty a => Int -> a -> Doc
vrow w = indent 1 . fill w . bold . pretty

instance Header ELB.LoadBalancerDescription where
    header _ = hcols 20
        [ H "dns:"
        ]

instance Body ELB.LoadBalancerDescription where
    body ELB.LoadBalancerDescription{..} = hcols 20
        [ C (fromMaybe "<blank>" lbdDNSName)
        ]

instance Title ASG.AutoScalingGroup where
    title ASG.AutoScalingGroup{..} = title asgAutoScalingGroupName

instance Header ASG.AutoScalingGroup where
    header _ = hcols 10
        [ W 8 (H "status:")
        , W 8 (H "weight:")
        , H "version:"
        , W 19 (H "zones:")
        , H "cooldown:"
        , H "[m..N]:"
        , H "desired:"
        , W 19 (H "created:")
        ]

instance Body (Ann ASG.AutoScalingGroup) where
    body (Ann ASG.AutoScalingGroup{..} Tags{..}) = hcols 10
        [ W 8 (C $ maybe "OK" pretty asgStatus)
        , W 8 (C tagWeight)
        , C tagVersion
        , W 19 (C asgAvailabilityZones)
        , C asgDefaultCooldown
        , C (pretty asgMinSize <> ".." <> pretty asgMaxSize)
        , C asgDesiredCapacity
        , W 19 (C asgCreatedTime)
        ]

instance Title EC2.DescribeImagesResponseItemType where
    title = title . EC2.diritName

instance Header EC2.DescribeImagesResponseItemType where
    header _ = hcols 14
        [ H "image-id:"
        , H "owner-id:"
        , H "state:"
        , H "public:"
        , H "root-device:"
        , W 30 (H "block-devices:")
        ]

instance Body EC2.DescribeImagesResponseItemType where
    body EC2.DescribeImagesResponseItemType{..} = hcols 14
        [ C diritImageId
        , C diritImageOwnerId
        , C diritImageState
        , C diritIsPublic
        , C diritRootDeviceType
        , W 30 (C devices)
        ]
      where
        devices = hsep $ map (pretty . EC2.bdmitDeviceName)
            diritBlockDeviceMapping

instance Header EC2.RunningInstancesItemType where
    header _ = hcols 15
        [ W 8 (H "state:")
        , W 8 (H "weight:")
        , H "instance-id:"
        , H "image-id:"
        , H "public-ip:"
        , H "type:"
        , W 19 (H "launched:")
        ]

instance Body (Ann EC2.RunningInstancesItemType) where
    body (Ann EC2.RunningInstancesItemType{..} Tags{..}) = hcols 15
        [ W 8 (C $ EC2.istName riitInstanceState)
        , W 8 (C tagWeight)
        , C riitInstanceId
        , C riitImageId
        , C (fromMaybe "" riitIpAddress)
        , C riitInstanceType
        , W 19 (C riitLaunchTime)
        ]

instance Title EC2.SecurityGroupItemType where
    title = title . EC2.sgitGroupName

instance Body EC2.SecurityGroupItemType where
    body EC2.SecurityGroupItemType{..} = vsep
        [ w "owner-id:"              <+> pretty sgitOwnerId
        , w "group-id:"              <+> pretty sgitGroupId
        , w "group-description:"     <+> pretty sgitGroupDescription
        , w "vpc-id:"                <+> pretty (fromMaybe "<blank>" sgitVpcId)
        , w "ip-permissions-egress:" <+> pretty sgitIpPermissionsEgress
        , w "ip-permissions:"        <+> pretty sgitIpPermissions
        ]
      where
        w = vrow 24

instance Title IAM.Role where
    title = title . IAM.rRoleName

instance Body IAM.Role where
    body IAM.Role{..} = vsep hs <-> policy
      where
        w  = vrow 23

        hs = [ w "arn:"         <+> pretty rArn
             , w "role-id:"     <+> pretty rRoleId
             , w "path:"        <+> pretty rPath
             , w "create-date:" <+> pretty rCreateDate
             ]

        policy = w "assume-policy-document:" <+>
            (pretty . prettyJSON $ decodeURL <$> rAssumeRolePolicyDocument)

instance Body IAM.GetRolePolicyResult where
    body IAM.GetRolePolicyResult{..} = vrow 23 "policy-document:" <+>
        (pretty . prettyJSON $ decodeURL grprPolicyDocument)

instance Title IAM.ServerCertificateMetadata where
    title = title . IAM.scmServerCertificateName

instance Header IAM.ServerCertificateMetadata where
    header _ = hcols 19
        [ W 59 (H "arn:")
        , W 22 (H "id:")
        , H "uploaded-at:"
        ]

instance Body IAM.ServerCertificateMetadata where
    body IAM.ServerCertificateMetadata{..} = hcols 19
        [ W 59 (C scmArn)
        , W 22 (C scmServerCertificateId)
        , C scmUploadDate
        ]

instance Title R53.HostedZone where
    title = title . R53.hzName

instance Header R53.HostedZone where
    header _ = hcols 10
        [ W 38 (H "id:")
        , W 38 (H "reference:")
        , H "config:"
        , H "record-count:"
        ]

instance Body R53.HostedZone where
    body R53.HostedZone{..} = hcols 10
        [ W 38 (C hzId)
        , W 38 (C hzCallerReference)
        , C hzConfig
        , C hzResourceRecordSetCount
        ]

instance Header R53.ResourceRecordSet where
    header _ = hcols 10
        [ W 36 (H "name:")
        , W 7 (H "type:")
        , H "region:"
        , W 7 (H "ttl:")
        , W 7 (H "weight:")
        , H "failover:"
        , W 32 (H "set-id:")
        , W 36 (H "value:")
        ]

instance Body R53.ResourceRecordSet where
    body x = hcols 10
        [ W 36 . C . R . B $ R53.rrsName x
        , W 7 . C $ R53.rrsType x
        , region x
        , W 7 (ttl x)
        , W 7 (weight x)
        , failover x
        , W 32 (set x)
        , W 36 (value x)
        ]
      where
        region R53.LatencyRecordSet      {..} = C rrsRegion
        region R53.LatencyAliasRecordSet {..} = C rrsRegion
        region _                              = Z

        ttl R53.FailoverRecordSet {..} = C rrsTTL
        ttl R53.LatencyRecordSet  {..} = C rrsTTL
        ttl R53.WeightedRecordSet {..} = C rrsTTL
        ttl R53.BasicRecordSet    {..} = C rrsTTL
        ttl _                          = Z

        weight R53.WeightedRecordSet      {..} = C rrsWeight
        weight R53.WeightedAliasRecordSet {..} = C rrsWeight
        weight _                               = Z

        failover R53.FailoverRecordSet      {..} = C rrsFailover
        failover R53.FailoverAliasRecordSet {..} = C rrsFailover
        failover _                               = Z

        set R53.BasicRecordSet{} = Z
        set R53.AliasRecordSet{} = Z
        set _ = C $ strip (R53.rrsSetIdentifier x)

        value R53.FailoverAliasRecordSet {..} = C rrsAliasTarget
        value R53.LatencyAliasRecordSet  {..} = C rrsAliasTarget
        value R53.WeightedAliasRecordSet {..} = C rrsAliasTarget
        value R53.AliasRecordSet         {..} = C rrsAliasTarget
        value _ = C (map strip . R53.rrValues $ R53.rrsResourceRecords x)

        strip = stripText ".compute" . stripText ".amazonaws.com"

instance Pretty Builder where
    pretty = pretty . LText.toStrict . LText.toLazyText

instance Pretty Text where
    pretty = text . Text.unpack

instance Pretty UTCTime where
    pretty = text . formatUTC

instance Pretty Region where
    pretty = text . show

instance Pretty FilePath where
    pretty = text . Path.encodeString

instance Pretty Version where
    pretty = pretty . Ver.toText

instance Pretty AvailabilityZone where
    pretty AZ{..} = pretty azRegion <> char azSuffix

    prettyList []     = mempty
    prettyList [a]    = pretty a
    prettyList (a:as) = pretty (azRegion a) <> char ':' <+>
        hcat (punctuate comma $ map (char . azSuffix) as)

instance Pretty ASG.Filter where
    pretty ASG.Filter{..} = pretty fName <> char ':' <> pretty fValues

instance Pretty EC2.InstanceType where
    pretty = fromString . show

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
        , PP.bool atEvaluateTargetHealth
        ]

instance Pretty R53.ResourceRecords where
    pretty = pretty . R53.rrValues

instance Pretty R53.Config where
    pretty = pretty . fromMaybe "<blank>" . R53.cComment

instance Pretty R53.Failover where
    pretty = text . show

instance Pretty Env where
    pretty = pretty . _env

instance Pretty Role where
    pretty = pretty . _role

prettyJSON :: ToJSON a => Maybe a -> Text
prettyJSON = Text.decodeUtf8 . LBS.toStrict . maybe "" Aeson.encodePretty

decodeURL :: Text -> Maybe Object
decodeURL = decode . LBS.fromStrict . urlDecode True . Text.encodeUtf8

reproxy :: proxy s -> Proxy t
reproxy _ = Proxy
