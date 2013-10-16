{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Khan.CLI.Chef
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.CLI.Chef (cli) where

import           Data.Aeson
import           Data.Attoparsec.Text
import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Text.Format
import qualified Data.Text.IO              as Text
import qualified Filesystem.Path.CurrentOS as Path
import qualified Khan.AWS.EC2              as EC2
import qualified Khan.AWS.IAM              as IAM
import           Khan.Internal
import           Khan.Prelude
import           Network.AWS
import           Network.AWS.EC2
import qualified Shelly                    as Shell
import           System.Random             (randomRIO)

cookbookRole :: Text -> AWS Text
cookbookRole def
    | not $ invalid def = return def
    | otherwise         = liftEitherT $ do
        p <- sh $ Shell.test_e "metadata.rb"
        r <- if p
                 then fromMaybe def <$> match
                 else return def
        let r' = fromMaybe r $ Text.stripSuffix "-role" r
        logInfo "Using role '{}'" [r']
        return r'
  where
    key = "name"

    match = liftIO $ do
        logInfo_ "Reading metadata.rb"
        ls <- Text.lines <$> Text.readFile "metadata.rb"
        return . join
               . fmap (maybeResult . parse parser)
               $ find (key `Text.isPrefixOf`) ls

    parser = (string key >> skipSpace >> satisfy separator)
        *> takeTill separator

    separator c = c == '\'' || c == '"'

defineOptions "Launch" $ do
    textOption "lRole" "role" ""
        "Instance's role."

    textOption "lEnv" "env" defaultEnv
        "Instance's environment."

    textOption "lDomain" "domain" ""
        "Instance's DNS domain."

    maybeTextOption "lImage" "image" ""
        "Id of the image/ami."

    integerOption "lMin" "min" 1
        "Minimum number of instances to launch."

    integerOption "lMax" "max" 1
        "Maximum number of instances to launch."

    textsOption "lGroups" "groups" []
        "Security groups."

    pathOption "lData" "user-data" ""
        "Path to user data file."

    instanceTypeOption "lType" "type" M1_Small
        "Instance's type."

    boolOption "lOptimised" "optimised" False
        "EBS optimisation."

    rulesOption "lRules" "rules"
        "IP permission specifications."

    stringOption "lZones" "zones" "abc"
         "Availability zones suffixes to provision into (psuedo-random)."

    textOption "lUser" "user" ""
        "SSH user."

    intOption "lTimeout" "timeout" 60
        "SSH timeout."

    -- Block Device Mappings
    -- Monitoring
    -- Disable Api Termination
    -- Instance Shutdown Behavior
    -- Client Token
    -- Network Interfaces

deriving instance Show Launch

instance Discover Launch where
    discover l@Launch{..} = do
        ud <- defaultDataFile lData "user-data"
        return $! l { lData = ud }

instance Validate Launch where
    validate Launch{..} = do
        check lRole   "--role must be specified."
        check lEnv    "--env must be specified."
        check lDomain "--domain must be specified."
        check lMin    "--min must be greater than 0."
        check lMax    "--max must be greater than 0."
        check lZones  "--zones must be specified."

        check (not $ lMin <= lMax)    "--min must be less than or equal to --max."
        check (Within lZones "abcde") "--zones must be within [a-e]."

        checkPath lData " specified by --user-data must exist."

instance Naming Launch where
    names Launch{..} = unversioned lRole lEnv

defineOptions "Bundle" $ do
    textOption "tRole" "role" ""
        "Instance's role."

    pathOption "tSolo" "solo" ""
        "Chef solo.rb configuration to use."

    pathOption "tTmp" "tmp" ".khan"
        "Temporary working directory."

deriving instance Show Bundle

instance Discover Bundle where
    discover t@Bundle{..} = do
        s <- defaultDataFile tSolo "solo.rb"
        r <- cookbookRole tRole
        return $! t { tRole = r, tSolo = s }

instance Validate Bundle where
    validate Bundle{..} = do
        check tRole "--role must be specified."
        checkPath tSolo " specified by --solo must exist."
        checkPath tTmp  " specified by --tmp must exist."

instance ToJSON Bundle where
    toJSON Bundle{..} = object
        [ "run_list" .= toJSON [format "recipe[{}]" [tRole]]
        ]

defineOptions "Host" $ do
    textOption "hRole" "role" ""
        "Instance's role."

    textOption "hEnv" "env" defaultEnv
        "Instance's environment."

    textsOption "hHosts" "hosts" []
        "Hosts to run on."

    pathOption "hBundle" "bundle" ""
        "Path to the bundle."

deriving instance Show Host

instance Discover Host

instance Validate Host where
    validate Host{..} = do
        if invalid hHosts
            then do
                check hRole "--role must be specified."
                check hEnv  "--env must be specified."
            else check hHosts "--hosts must be specified."
        checkPath hBundle " specified by --bundle must exist."

cli :: Command
cli = Command "chef" "Manage Chef EC2 Instances."
    [ subCommand "launch" launch
    , subCommand "bundle" bundle
    , subCommand "run"    run
    , subCommand "stop"   stop
    ]

launch :: Launch -> AWS ()
launch l@Launch{..} = do
    ami <- maybe (EC2.findImage [roleName, "base"]) return lImage

    i <- async $ IAM.findRole l
    k <- async $ EC2.createKey l
    s <- async $ EC2.updateGroup (sshGroup lEnv) sshRules
    g <- async $ EC2.updateGroup l lRules

    wait_ i <* logInfo "Found IAM Profile {}" [profileName]
    wait_ k <* logInfo "Found KeyPair {}" [keyName]
    wait_ s <* logInfo "Found SSH Group {}" [sshGroup lEnv]
    wait_ g <* logInfo "Found Role Group {}" [groupName]

    ud  <- Text.decodeUtf8 . Base64.encode <$> shell (Shell.readBinary lData)
    az  <- shuffle lZones
    reg <- currentRegion
    ms1 <- EC2.runInstances l ami lType (AZ reg az) lMin lMax ud lOptimised

    let ids = map riitInstanceId ms1

    EC2.waitForInstances ids
    EC2.tagInstances l lDomain ids
  where
    Names{..} = names l

bundle :: Bundle -> AWS ()
bundle t@Bundle{..} = liftEitherT $ do
    exists "Berksfile"
    exists "chefignore"
    sh $ do
        Shell.rm_rf tTmp >> Shell.mkdir_p tTmp
        Shell.cp tSolo solo

    sync . LBS.writeFile (Path.encodeString node) $ encode t

    sh $ do
        Shell.run_ "bundle" ["exec", "berks", "install", "--path", path books]
        b <- Shell.absPath output
        Shell.chdir tTmp $ Shell.run_ "tar" ["zcf", path b, "."]
        logInfo "Bundle created at {}" [path b]
  where
    output = "bundle.tar.gz" :: FilePath

    books = tTmp </> ("cookbooks" :: FilePath)
    solo  = tTmp </> ("solo.rb"   :: FilePath)
    node  = tTmp </> ("node.json" :: FilePath)

    exists f = sh (Shell.test_e f) >>=
        assert "Missing {}, is the current dir a Chef Cookbook?" [path f] . not

run :: Host -> AWS ()
run Host{..} = do
    hs <- if null hHosts
              then liftEitherT . mapM dns =<< EC2.findInstances []
                  [ tag roleTag roleName
                  , tag envTag  envName
                  ]
              else return hHosts

    logInfo "Running on hosts: \n{}" [Text.intercalate "\n" hs]
  where
    tag key = Filter ("tag:" <> key) . (:[])

    Names{..} = unversioned hRole hEnv

    dns RunningInstancesItemType{..} = riitDnsName ??
        (Text.unpack $ "Blank public DNS for " <> riitInstanceId)

    -- Describe instances
    -- SCP bundle to public DNS
    -- SSH in and:
    --   get + execute user-data, or should it just be done here ignoring need for user-data?
    --   untar bundle and run chef-solo

    -- ms2 <- EC2.findInstances ids
    -- ts  <- catMaybes <$> mapM formulate ms2

    -- logInfo_ "Running SSH tasks.."
    -- liftIO . runEffect $ for (runTasks ts lTimeout) (liftIO . print)

    -- formulate RunningInstancesItemType{..} =
    --     case riitDnsName of
    --         Nothing -> do
    --             logError "No public DNS found for instance {}" [riitInstanceId]
    --             return Nothing
    --         Just x  -> do
    --             logInfo "Instance {} located at {}" [riitInstanceId, x]
    --             return . Just $ Task (Text.unpack x) ["cat /etc/hostname"]

    -- let port        = 22
    --     known_hosts = home </> ".ssh" </> "known_hosts"
    --     public      = home </> ".ssh" </> "id_rsa.pub"
    --     private     = home </> ".ssh" </> "id_rsa"

    -- withSSH2 known_hosts public private "" login host 22 $ s ->
    --     withChannel s $ \ch -> do
    --         channelExecute ch command
    --         result <- readAllChannel ch
    --         BSL.putStr result

stop :: Host -> AWS ()
stop Host{..} = return ()

shuffle :: MonadIO m => [a] -> m a
shuffle xs = liftIO $ randomRIO (0, length xs - 1) >>= return . (xs !!)
