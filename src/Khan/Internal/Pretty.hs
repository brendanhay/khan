{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty     as Aeson
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.String
import qualified Data.Text                    as Text
import           Data.Text.Buildable
import qualified Data.Text.Encoding           as Text
import           Data.Text.Format             (Only(..))
import qualified Data.Text.Lazy.Builder       as LText
import           Data.Time                    (UTCTime)
import qualified Khan.Model.Tag               as Tag
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

vcols :: Int -> [Column] -> Doc
vcols w = vsep . cols w

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

instance Pretty IAM.Role where
    pretty IAM.Role{..} = vsep hs <-> policy
      where
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
             ]

instance Pretty AvailabilityZone where
    pretty AZ{..} = pretty azRegion <> char azSuffix

    prettyList []     = mempty
    prettyList [a]    = pretty a
    prettyList (a:as) = pretty (azRegion a) <> prettyList (map azSuffix as)

instance Pretty EC2.RunningInstancesItemType where
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
