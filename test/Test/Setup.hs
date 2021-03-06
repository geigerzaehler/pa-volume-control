{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

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

import System.Directory (removeFile, removeDirectoryRecursive)
import System.Environment
import System.FilePath
import System.Process
import System.Posix.Signals
import System.Posix.Types
import System.IO.Temp (createTempDirectory, emptySystemTempFile, getCanonicalTemporaryDirectory)

import qualified DBus
import qualified DBus.Client as DBus


type Notification = (Word32, String)


-- | Prepare the test environment
--
-- * Launch a mock DBus notifaction server
-- * Manipulate PATH so that the mock `pacmd` command and
-- `pa-volume-control` are available.
setup :: (MonadIO m, MonadResource m) => m (TVar Notification)
setup = do
    liftIO prepareSearchPath
    setupCacheDirectory
    setupPacmdMock
    setupDbus


-- | Make sure that the local build of 'pa-volume-control' and the
-- 'pacmd' stub command are available from the search path.
--
-- TODO Remove this and give the full path when running the
-- pa-volume-control command.
prepareSearchPath :: IO ()
prepareSearchPath  = do
    cabalBuildDir <- lookupEnv "CABAL_BUILD_DIR"
    haskellDistDir <- lookupEnv "HASKELL_DIST_DIR"
    buildDir <- maybe
        (error "Either the CABAL_BUILD_DIR or the HASKELL_DIST_DIR environment variables must be set")
        return
        (cabalBuildDir <|> haskellDistDir)
    prependSearchPath $ buildDir </> "pa-volume-control"


-- | Make the mock @pacmd@ available in the search path and return the
-- file patch for the state file
setupPacmdMock :: (MonadIO m, MonadResource m) => m ()
setupPacmdMock = do
    (_, filePath) <- allocate
        (emptySystemTempFile "pa-volume-control-test-pa-state")
        removeFile
    liftIO $ setEnv "PACMD_STATE_FILE" filePath
    liftIO $ prependSearchPath "./test/bin-stubs"
    return ()


prependSearchPath :: FilePath -> IO ()
prependSearchPath path = do
    searchPath <- getEnv "PATH"
    let newSearchPath = path ++ [searchPathSeparator] ++ searchPath
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
    (_, (lastNotification, _)) <- allocate exportNotificationService unexportNotificationService
    return lastNotification

launchDbus :: IO (String, ProcessID)
launchDbus = do
    output <-readProcess "dbus-launch" [] []
    let Just address = asum $ stripPrefix "DBUS_SESSION_BUS_ADDRESS=" <$> lines output
    let Just pid = asum $ stripPrefix "DBUS_SESSION_BUS_PID=" <$> lines output
    return (address, read pid)


killDbus :: (a, ProcessID) -> IO ()
killDbus (_, pid) = signalProcess sigTERM pid


exportNotificationService :: IO (TVar Notification, DBus.Client)
exportNotificationService = do
    lastNotification <- liftIO $ newTVarIO (0, "")
    client <- DBus.connectSession
    _ <- DBus.requestName client "org.freedesktop.Notifications" []
    DBus.export client "/" $ notificationInterface lastNotification
    return (lastNotification, client)
  where
    notificationInterface :: TVar Notification -> DBus.Interface
    notificationInterface lastNotification = DBus.Interface
        { DBus.interfaceName = "org.freedesktop.Notifications"
        , DBus.interfaceMethods = [ notify lastNotification ]
        , DBus.interfaceProperties = []
        , DBus.interfaceSignals = []
        }

    notify :: TVar Notification -> DBus.Method
    notify lastNotification = DBus.Method
        { DBus.methodName = "Notify"
        , DBus.inSignature = fromJust $ DBus.parseSignature "susssasa{sv}i"
        , DBus.outSignature = fromJust $ DBus.parseSignature "u"
        , DBus.methodHandler = dbusNotify lastNotification
        }

unexportNotificationService :: (a, DBus.Client) -> IO ()
unexportNotificationService (_, client)= do
    DBus.unexport client "/"
    DBus.disconnect client


-- | Mock implementation of the DBus Notify method that just updates
-- the notifcation 'TVar'.
dbusNotify :: MonadIO m => TVar Notification -> DBus.MethodCall -> m DBus.Reply
dbusNotify lastNotification methodCall = do
    let _ :replacesId:_:summary: _ = DBus.methodCallBody methodCall
    liftIO $ atomically $ writeTVar lastNotification (fromJust $ DBus.fromVariant replacesId, fromJust $ DBus.fromVariant summary)
    return $ DBus.ReplyReturn [ DBus.toVariant (1 :: Word32) ]
