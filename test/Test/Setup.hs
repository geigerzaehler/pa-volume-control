{-# LANGUAGE OverloadedStrings #-}

module Test.Setup (
    setup
  , Notification
) where


import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Foldable
import Data.Maybe
import Data.List
import Data.Word

import System.Directory (removeDirectoryRecursive)
import System.Environment
import System.FilePath
import System.Process
import System.Posix.Signals
import System.Posix.Types
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

import qualified DBus as DBus
import qualified DBus.Client as DBus


type Notification = (Word32, String)


-- | Prepare the test environment
--
-- * Launch a mock DBus notifaction server
-- * Manipulate PATH so that the mock `pacmd` command and
-- `pa-volume-control` are available.
setup :: (MonadIO m, MonadResource m) => m (TVar Notification)
setup = do
    liftIO $ do
        prepareSearchPath
        setEnv "PACMD_STATE_FILE" "pa-state~"
    setupCacheDirectory
    setupDbus


-- | Make sure that the local build of 'pa-volume-control' and the
-- 'pacmd' stub command are available from the search path.
prepareSearchPath :: IO ()
prepareSearchPath  = do
    cabalBuildDir <- lookupEnv "CABAL_BUILD_DIR"
    haskellDistDir <- lookupEnv "HASKELL_DIST_DIR"
    buildDir <- maybe
        (error "Either the CABAL_BUILD_DIR or the HASKELL_DIST_DIR environment variables must be set")
        return
        (cabalBuildDir <|> haskellDistDir)

    let additionalPaths =
            [ buildDir </> "pa-volume-control"
            , "./test/bin-stubs"
            ]
    searchPath <- getEnv "PATH"
    let newSearchPath = intercalate [ searchPathSeparator ] $ additionalPaths ++ [ searchPath ]
    setEnv "PATH" newSearchPath


-- | Create a temporary directory to use as XDG_CACHE_HOME
setupCacheDirectory :: (MonadIO m, MonadResource m) => m ()
setupCacheDirectory = do
    (_, cacheDir) <- allocate createDir removeDirectoryRecursive
    liftIO $ setEnv "XDG_CACHE_HOME" cacheDir
    where
        createDir = do
            tmpDir <- getCanonicalTemporaryDirectory
            createTempDirectory tmpDir "pa-volume-control-test-cache"


-- * DBus
--

-- | Launches a DBus server and a mock notificaiton service.
--
-- Returns a 'TVar' that holds the most recent notification, that is a
-- notification ID and message pair.
setupDbus :: (MonadIO m, MonadResource m) => m (TVar Notification)
setupDbus = do
    (_, (address, _)) <- allocate launchDbus killDbus
    liftIO $ setEnv "DBUS_SESSION_BUS_ADDRESS" address
    (_, (lastNotification, _)) <- allocate exportNotificationService (unexportNotificationService)
    return lastNotification

launchDbus :: IO (String, ProcessID)
launchDbus = do
    output <-readProcess "dbus-launch" [] []
    let Just address = asum $ fmap (stripPrefix "DBUS_SESSION_BUS_ADDRESS=") $ lines output
    let Just pid = asum $ fmap (stripPrefix "DBUS_SESSION_BUS_PID=") $ lines output
    return (address, read pid)


killDbus :: (a, ProcessID) -> IO ()
killDbus (_, pid) = signalProcess sigTERM pid


exportNotificationService :: IO (TVar Notification, DBus.Client)
exportNotificationService = do
    lastNotification <- liftIO $ newTVarIO (0, "")
    client <- DBus.connectSession
    _ <- DBus.requestName client "org.freedesktop.Notifications" []
    DBus.export client "/"
        [ DBus.method
          "org.freedesktop.Notifications"
          "Notify"
          (fromJust $ DBus.parseSignature "susssasa{sv}i")
          (fromJust $ DBus.parseSignature "u")
          (dbusNotify lastNotification)
        ]
    return (lastNotification, client)


unexportNotificationService :: (a, DBus.Client) -> IO ()
unexportNotificationService (_, client)= do
    DBus.unexport client "/"
    DBus.disconnect client


-- | Mock implementation of the DBus Notify method that just updates
-- the notifcation 'TVar'.
dbusNotify :: TVar Notification -> DBus.MethodCall -> IO DBus.Reply
dbusNotify lastNotification methodCall = do
    let _ :replacesId:_:summary: _ = DBus.methodCallBody methodCall
    atomically $ writeTVar lastNotification (fromJust $ DBus.fromVariant replacesId, fromJust $ DBus.fromVariant summary)
    return $ DBus.replyReturn [ DBus.toVariant (1 :: Word32) ]
