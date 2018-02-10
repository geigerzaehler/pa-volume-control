{-# LANGUAGE OverloadedStrings #-}

module Test.Setup (
    setup
) where


import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import           Data.Foldable
import           Data.Maybe
import           Data.List
import           Data.Word

import System.Environment
import System.Process
import System.Posix.Signals
import System.Posix.Types

import qualified DBus as DBus
import qualified DBus.Client as DBus


type Notification = (Word32, String)


setup :: (MonadIO m, MonadResource m) => m (TVar Notification)
setup = do
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
          (runNotify lastNotification)
        ]
    return (lastNotification, client)


unexportNotificationService :: (a, DBus.Client) -> IO ()
unexportNotificationService (_, client)= do
    DBus.unexport client "/"
    DBus.disconnect client


runNotify :: TVar Notification -> DBus.MethodCall -> IO DBus.Reply
runNotify lastNotification methodCall = do
    let _ :replacesId:_:summary: _ = DBus.methodCallBody methodCall
    atomically $ writeTVar lastNotification (fromJust $ DBus.fromVariant replacesId, fromJust $ DBus.fromVariant summary)
    return $ DBus.replyReturn [ DBus.toVariant (1 :: Word32) ]
