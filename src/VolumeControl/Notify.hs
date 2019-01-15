{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VolumeControl.Notify (
    notifyVolume
) where


import           Control.Monad

import           Data.Int (Int32)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Word (Word32)

import qualified DBus
import qualified DBus.Client as DBus

import           System.Directory
import qualified System.Environment.XDG.BaseDir as Xdg
import           System.FilePath


notifyVolume :: Int -> Bool -> IO ()
notifyVolume volume isMuted = do
    notificationId <- readNotificationId
    client <- DBus.connectSession
    let label = if isMuted then "Muted" else "Volume"
    newNotificationId <- notify client $ makeNotification
        { summary = label <> " " <> show volume <> "%" :: String
        , body = volumeBar isMuted volume
        , replacesId = notificationId }
    when (newNotificationId /= notificationId) $
        writeNotificationId newNotificationId



data Notification = Notification
    { appName :: String
    , replacesId :: Word32
    , icon :: String
    , summary :: String
    , body :: String
    , actions :: [String]
    , hints :: Map.Map String DBus.Variant
    , expiration :: Int32 }


makeNotification :: Notification
makeNotification =
    Notification
    { appName = ""
    , replacesId = 0
    , icon = ""
    , summary = ""
    , body = ""
    , actions = []
    , hints = Map.empty
    , expiration = -1 }


notify :: DBus.Client -> Notification -> IO Word32
notify client Notification { .. } = do
    reply <- DBus.call_ client
        (DBus.methodCall "/" "org.freedesktop.Notifications" "Notify")
        { DBus.methodCallDestination = Just "org.freedesktop.Notifications"
        , DBus.methodCallBody =
            [ DBus.toVariant appName
            , DBus.toVariant replacesId
            , DBus.toVariant icon
            , DBus.toVariant summary
            , DBus.toVariant body
            , DBus.toVariant actions
            , DBus.toVariant hints
            , DBus.toVariant expiration
            ]
        }
    let returnValue = head $ DBus.methodReturnBody reply
    return $ fromJust $ DBus.fromVariant returnValue


--
-- * Notifiction ID cache
--

notificationIdCacheFile :: IO FilePath
notificationIdCacheFile = Xdg.getUserCacheFile "pa-volume-control" "notification-id"

writeNotificationId :: Word32 -> IO ()
writeNotificationId i = do
    file <- notificationIdCacheFile
    createDirectoryIfMissing False (takeDirectory file)
    writeFile file (show i)


readNotificationId :: IO Word32
readNotificationId = do
    file <- notificationIdCacheFile
    exists <- doesFileExist file
    if not exists
    then
        return 0
    else
        read <$> readFile file


--
-- * Message generation
--

squareFull :: String
squareFull = "\x25a0"

squareEmpty :: String
squareEmpty = "\x25a1"

squareCross :: String
squareCross = "\x25a8"

volumeBar :: Bool -> Int -> String
volumeBar isMute percentage =
    let square = if isMute then squareCross else squareFull
        fullCount = percentage `div` 10
        emptyCount = 10 - fullCount
        full = concat (replicate fullCount square)
        rest = if emptyCount < 0
               then "|" <> concat (replicate (- emptyCount) square)
               else concat (replicate emptyCount squareEmpty)
    in full <> rest
