{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Main
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Data.List                (partition)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import qualified Khan.Model.Host          as Host
import qualified Khan.Model.SSH           as SSH
import           Khan.Prelude
import           Khan.Sync.Enumerate
import           Network.AWS.EC2.Metadata

-- data Action
--     = List Text [Action]
--     | Get  Text
--       deriving (Eq, Show)

main :: IO ()
main = do
    -- mapM_ Text.putStrLn
    --     . concatMap (pprint 0)
    --     . merge
    --     . actions
    --     $ (enumerate :: [Meta])

  where
    get url = do
        

curl http://169.254.169.254/latest/meta-data/

ami-id
ami-launch-index
ami-manifest-path
block-device-mapping/
hostname
instance-action
instance-id
instance-type
kernel-id
local-hostname
local-ipv4
mac
network/
placement/
public-hostname
public-ipv4
public-keys/
reservation-id
security-groups
services/

    -- actions :: ToPath a => [a] -> [[Action]]
    -- actions = map (build . Text.split (== '/') . toPath)

    -- build []     = []
    -- build [x]    = [Get  x]
    -- build (x:xs) = [List x (build xs)]

    -- merge :: [Action] -> [Action]
    -- merge = foldr' go []
    --   where
    --     go :: Action -> [Action] -> [Action]
    --     go (List w ws) xs =
    --         case partition (match w) xs of
    --             (ys, []) -> [List w (merge (ws ++ ys))]
    --             (ys, zs) -> List w (merge (ws ++ ys)) : zs
    --     go x xs = x : xs

    --     match :: Text -> Action -> Bool
    --     match x (List y _) = x == y
    --     match _ _          = False

    -- pprint :: Int -> Action -> [Text]
    -- pprint n x = case x of
    --     List k xs -> f k : concatMap (pprint (n + 2)) xs
    --     Get k     -> [f k]
    --   where
    --     f = mappend (Text.replicate n " ")

--    foldr (a -> b -> b) -> b -> t a -> b

--    mapM_ (print . toPath) (enumerate :: [Meta])

-- need to save userdata
-- need to list securitycredentials, save, get first, save


-- ec2 :: (Functor m, MonadIO m) => m Bool
-- ec2 = fmap isRight
--     . runEitherT
--     . syncIO
--     $ simpleHttp "http://instance-data/latest"

-- user :: MonadIO m => m (Maybe LBS.ByteString)
-- user = liftIO $ EX.catch (Just <$> simpleHttp url) ex
--   where
--     url = "http://169.254.169.254/latest/user-data"

--     ex (StatusCodeException s _ _)
--         | status404 == s = return Nothing
--     ex e                 = EX.throw e

-- meta :: (Functor m, MonadIO m) => Meta -> EitherT String m ByteString
-- meta = get "http://169.254.169.254/latest/meta-data/"

-- dynamic :: (Functor m, MonadIO m) => Dynamic -> EitherT String m ByteString
-- dynamic = get "http://169.254.169.254/latest/dynamic/"

-- get :: (Functor m, MonadIO m, ToPath a)
--     => Text
--     -> a
--     -> EitherT String m ByteString
-- get base p = do
--     rs <- fmapLT show
--         . syncIO
--         . simpleHttp
--         . Text.unpack
--         $ base <> toPath p

--     case strip '\n' $ LBS.toStrict rs of
--         "" -> throwError "Failed to receive any data"
--         bs -> return bs
