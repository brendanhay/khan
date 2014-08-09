{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

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
    -- * Idempotence predicate
      Changed

    -- * Capturing ansible output
    , capture

    -- * JSON output formatters
    , ImageInput (..)
    , Inv        (..)
    , Host       (..)

    -- * Inventory
    , inventoryPath

    -- * CLI arguments
    , (+$+)
    , overrides
    ) where

import           Control.Monad.Except
import qualified Data.Aeson.Encode.Pretty         as Aeson
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.List                        (intercalate)
import qualified Data.Text                        as Text
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Filesystem.Path.CurrentOS        as Path
import           Khan.Internal.AWS
import           Khan.Internal.Options
import           Khan.Internal.Types
import           Khan.Model.Ansible.Serialisation
import           Khan.Prelude
import           Network.AWS
import           System.Exit

class Changed a where
    changed :: a -> Bool

instance Changed Bool where
    changed = id

instance Changed (Modified a) where
    changed (Changed   _) = True
    changed (Unchanged _) = False

capture :: (Params ps, Changed a)
        => Bool
        -> Common
        -> Format
        -> ps
        -> AWS a
        -> AWS ()
capture ansible  c f ps aws = contextAWS c (aws >>= success . changed)
    >>= either failure return
    >>= exit
  where
    success True  = changed' (f <> " changed.") ps
    success False = unchanged (f <> " unchanged.") ps

    failure (Err s)  = failed "{}" $ Only s
    failure (Ex  ex) = failure . toError $ show ex
    failure (Ers es) = failure . toError . intercalate ", " $ map show es

    exit o = liftIO $ do
        when ansible $ LBS.putStrLn (Aeson.encodePretty o)
        case o of
            Fail     _               -> exitFailure
            NoChange _ | not ansible -> exitWith (ExitFailure 69)
            _                        -> exitSuccess

    changed'  g = return . Change   . format g
    unchanged g = return . NoChange . format g
    failed    g = return . Fail     . format g

inventoryPath :: CacheDir -> Env -> AWS FilePath
inventoryPath (CacheDir dir) env = do
    r <- Text.pack . show <$> getRegion
    return $ dir </> Path.fromText (Text.concat [r, "_", _env env])

overrides :: Naming a => a -> [String] -> [String]
overrides (names -> n) = (+$+ [("--extra-vars", Text.unpack quoted)])
  where
    quoted = "'" <> Text.intercalate " " vars <> "'"
    vars   = map (\(k, v) -> k <> "=" <> v) (extraVars n)

(+$+) :: [String] -> [(String, String)] -> [String]
(+$+) args extras = args ++ foldr' add [] extras
  where
    add (k, v) xs =
        if k `elem` args
            then xs
            else k : v : xs
