
{-# LANGUAGE ExtendedDefaultRules       #-}
<<<<<<< HEAD
{-# LANGUAGE GADTs                      #-}
=======
>>>>>>> b44920f... Adding pretty print instances for Role and Cluster info subcommand usage
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

<<<<<<< HEAD
=======
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
    , pp
    , ppi
    , ln
    , title
    ) where

>>>>>>> b44920f... Adding pretty print instances for Role and Cluster info subcommand usage
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.String
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import           Data.Text.Format             (Only(..))
<<<<<<< HEAD
=======
import qualified Data.Text.Lazy               as LText
>>>>>>> b44920f... Adding pretty print instances for Role and Cluster info subcommand usage
import           Data.Time                    (UTCTime)
import qualified Khan.Model.Tag               as Tag
import           Khan.Prelude
import           Network.AWS
import qualified Network.AWS.AutoScaling      as ASG
import qualified Network.AWS.EC2              as EC2
import qualified Network.AWS.IAM              as IAM
import           Network.HTTP.Types           (urlDecode)
<<<<<<< HEAD
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

default (Doc)

pp :: (MonadIO m, Pretty a) => a -> m ()
pp = ppi 0

ppi :: (MonadIO m, Pretty a) => Int -> a -> m ()
ppi i = log "{}" . Only . ($ "") . displayS . renderPretty 0.4 100 . indent i . pretty

ln :: MonadIO m => m ()
ln = liftIO (putStrLn "")

title :: Pretty a => a -> Doc
title n = lbracket <> bold (green $ pretty n) <> rbracket <+> "->"

data Col where
    H :: Pretty a => a -> Col
    C :: Pretty a => a -> Col
    D :: Pretty a => a -> Col

instance Pretty Col where
    pretty (H x) = pretty x
    pretty (C x) = pretty x
    pretty (D x) = pretty x

cols :: Int -> [Col] -> [Doc]
cols w = map f
  where
    f (H x) = fill w  (bold $ pretty x)
    f (C x) = fill w  (pretty x)
    f (D x) = fill 19 (pretty x)

hcols :: Int -> [Col] -> Doc
hcols w = hsep . cols w

vcols :: Int -> [Col] -> Doc
vcols w = vsep . cols w

(<->) :: Doc -> Doc -> Doc
(<->) = (PP.<$>)
=======
import qualified Text.PrettyPrint.Leijen.Text as PP
import           Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))
>>>>>>> b44920f... Adding pretty print instances for Role and Cluster info subcommand usage

default (Doc)

pp :: (MonadIO m, Pretty a) => a -> m ()
pp = ppi 0

ppi :: (MonadIO m, Pretty a) => Int -> a -> m ()
ppi i = log "{}" . Only . displayT . renderPretty 0.4 100 . indent i . pretty

ln :: MonadIO m => m ()
ln = liftIO (putStrLn "")

title :: Pretty a => a -> Doc
title n = lbracket <> pretty n <> rbracket <+> "->"

dol :: Pretty a => a -> Doc
dol = fill 19 . pretty

col :: Pretty a => a -> Doc
col = fill 15 . pretty

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

instance Pretty UTCTime where
    pretty = fromString . formatUTC

instance Pretty Region where
    pretty = fromString . show

instance Pretty IAM.Role where
    pretty IAM.Role{..} = vsep hs <-> policy
      where
<<<<<<< HEAD
        cl = fill 23 . bold . pretty

        hs = [ cl "arn:"         <+> pretty rArn
             , cl "role-id:"     <+> pretty rRoleId
             , cl "path:"        <+> pretty rPath
             , cl "create-date:" <+> pretty rCreateDate
             ]

        policy = cl "assume-policy-document:" <+>
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
=======
        col' :: Pretty a => a -> Doc
        col' = fill 23 . pretty

        hs = [ col' "arn:"         <+> pretty rArn
             , col' "role-id:"     <+> pretty rRoleId
             , col' "path:"        <+> pretty rPath
             , col' "create-date:" <+> pretty rCreateDate
             ]

        policy = col' "assume-policy-document:" <+>
            (pretty . prettyJSON $ decodeURL <$> rAssumeRolePolicyDocument)

instance Pretty IAM.GetRolePolicyResult where
    pretty IAM.GetRolePolicyResult{..} = fill 23 "policy-document:" <+>
            (pretty . prettyJSON $ decodeURL grprPolicyDocument)

instance Pretty ASG.AutoScalingGroup where
    pretty ASG.AutoScalingGroup{..} = hsep hs <-> hsep bs
      where
        hs = [ col "status:"
             , col "zones:"
             , col "cooldown:"
             , col "min:"
             , col "max:"
             , col "desired:"
             , dol "created:"
             ]

        bs = [ col (maybe "OK" pretty asgStatus)
             , col asgAvailabilityZones
             , col asgDefaultCooldown
             , col asgMinSize
             , col asgMaxSize
             , col asgDesiredCapacity
             , dol asgCreatedTime
>>>>>>> b44920f... Adding pretty print instances for Role and Cluster info subcommand usage
             ]

instance Pretty AvailabilityZone where
    pretty AZ{..} = pretty azRegion <> char azSuffix

    prettyList []     = mempty
    prettyList [a]    = pretty a
    prettyList (a:as) = pretty (azRegion a) <> prettyList (map azSuffix as)

instance Pretty EC2.RunningInstancesItemType where
<<<<<<< HEAD
    pretty EC2.RunningInstancesItemType{..} = hcols 15
        [ C (int . Tag.lookupWeight $ Tag.flatten riitTagSet)
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
=======
    pretty EC2.RunningInstancesItemType{..} = hsep
        [ col . int . Tag.lookupWeight $ Tag.flatten riitTagSet
        , col riitInstanceId
        , col riitImageId
        , col (fromMaybe "" $ riitIpAddress)
        , col riitInstanceType
        , col (EC2.istName riitInstanceState)
        , dol riitLaunchTime
        ]

    prettyList is = hsep hs <-> vsep (map pretty is)
      where
        hs = [ col "weight:"
             , col "instance-id:"
             , col "image-id:"
             , col "public-ip:"
             , col "type:"
             , col "state:"
             , dol "launched:"
             ]

instance Pretty EC2.InstanceType where
    pretty = fromString . show
