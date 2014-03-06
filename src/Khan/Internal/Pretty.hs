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
    ( Pretty     (..)
    , renderCompact

    , PrettyInst (..)
    , pp
    , ppi
    , ln
    , title
    ) where

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.List                    (sort)
import           Data.String
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Text.Format             (Only(..))
import           Data.Time                    (UTCTime)
import qualified Filesystem.Path.CurrentOS    as Path
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.AutoScaling      as ASG
import qualified Network.AWS.EC2              as EC2
import qualified Network.AWS.IAM              as IAM
import           Network.HTTP.Types           (urlDecode)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

default (Doc)

data Column where
    H :: Pretty a => a -> Column
    C :: Pretty a => a -> Column
    D :: Pretty a => a -> Column

instance Pretty Column where
    pretty (H x) = pretty x
    pretty (C x) = pretty x
    pretty (D x) = pretty x

cols :: Int -> [Column] -> [Doc]
cols w = map f
  where
    f (H x) = fill w  (bold $ pretty x)
    f (C x) = fill w  (pretty x)
    f (D x) = fill 19 (pretty x)

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

instance Pretty ASG.AutoScalingGroup where
    pretty ASG.AutoScalingGroup{..} = hcols w hs <-> hcols w bs
      where
        w  = 15

        hs = [ H "status:"
             , H "zones:"
             , H "cooldown:"
             , H "min:"
             , H "max:"
             , H "desired:"
             , H (D "created:")
             ]

        bs = [ C (maybe "OK" pretty asgStatus)
             , C asgAvailabilityZones
             , C asgDefaultCooldown
             , C asgMinSize
             , C asgMaxSize
             , C asgDesiredCapacity
             , C (D asgCreatedTime)
             ]

instance Pretty AvailabilityZone where
    pretty AZ{..} = pretty azRegion <> char azSuffix

    prettyList []     = mempty
    prettyList [a]    = pretty a
    prettyList (a:as) = pretty (azRegion a) <> prettyList (map azSuffix as)

data PrettyInst = PI EC2.RunningInstancesItemType Int

instance Pretty PrettyInst where
    pretty (PI EC2.RunningInstancesItemType{..} weight) = hcols 15
        [ C (int weight)
        , C riitInstanceId
        , C riitImageId
        , C (fromMaybe "" $ riitIpAddress)
        , C riitInstanceType
        , C (EC2.istName riitInstanceState)
        , C (D riitLaunchTime)
        ]

    prettyList is = hcols 15 hs <-> vsep (map pretty is)
      where
        hs = [ H "weight:"
             , H "instance-id:"
             , H "image-id:"
             , H "public-ip:"
             , H "type:"
             , H "state:"
             , H (D "launched:")
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
