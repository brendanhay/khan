{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

-- Module      : Khan.Model.Ansible
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Model.Ansible
    (
    -- * Idempotence check
      Modified (..)

    -- * Running ansible
    , capture

    -- * Output wrappers
    , Inv  (..)
    , Host (..)

    -- * Inventory
    , inventoryPath

    -- * CLI arguments
    , (+$+)
    , extraVars
    ) where

import           Control.Monad.Error
import qualified Data.Aeson.Encode.Pretty    as Aeson
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.List                   (intercalate)
import qualified Data.Text                   as Text
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Filesystem.Path.CurrentOS   as Path
import           Khan.Internal.AWS
import           Khan.Internal.IO
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Model.Ansible.Internal
import           Khan.Prelude
import           Network.AWS
import           System.Exit

class Modified a where
    modified :: a -> Bool

instance Modified Bool where
    modified = id

capture :: (Params ps, Modified a)
        => Bool
        -> Common
        -> Format
        -> ps
        -> AWS a
        -> AWS ()
capture False _ _ _  aws = void aws
capture True  c f ps aws = contextAWS c (aws >>= success . modified)
    >>= either failure return
    >>= exit
  where
    success True  = changed (f <> " changed.") ps
    success False = unchanged (f <> " unchanged.") ps

    failure (Err s)  = failed "{}" $ Only s
    failure (Ex  ex) = failure . toError $ show ex
    failure (Ers es) = failure . toError . intercalate ", " $ map show es

    exit o = liftIO $ LBS.putStrLn (Aeson.encodePretty o) >>
        case o of
            Failed _ -> exitFailure
            _        -> exitSuccess

    changed   g = return . Changed   . format g
    unchanged g = return . Unchanged . format g
    failed    g = return . Failed    . format g

inventoryPath :: CacheDir -> Env -> AWS FilePath
inventoryPath (CacheDir dir) env = do
    r <- Text.pack . show <$> getRegion
    return $ dir </> Path.fromText (Text.concat [r, "_", _env env])

extraVars :: Naming a => a -> Region -> [String] -> [String]
extraVars (names -> Names{..}) reg = (+$+ [("--extra-vars", vars)])
  where
    vars = concat
        [ "'"
        , "khan_region="
        , show reg
        , " khan_region_abbrev="
        , Text.unpack (abbreviate reg)
        , " khan_env="
        , Text.unpack envName
        , " khan_key="
        , Text.unpack keyName
        , "'"
        ]

(+$+) :: [String] -> [(String, String)] -> [String]
(+$+) args extras = args ++ foldr' add [] extras
  where
    add (k, v) xs =
        if k `elem` args
            then xs
            else k : v : xs
