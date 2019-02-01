{-# LANGUAGE RecordWildCards #-}

module VolumeControl.Command (
    execCommand
  , Command (..)
) where

import VolumeControl.PulseAudio
import VolumeControl.Notify


data Command
    = VolumeUp { volumeUpNoLimit :: Bool }
    | VolumeDown
    | ToggleMute
    | ToggleSourceMute
    deriving (Show)


execCommand :: Command -> IO ()
execCommand (VolumeUp noLimit) = volChange noLimit 10
execCommand VolumeDown = volChange True (-10)
execCommand ToggleMute = toggleMute
execCommand ToggleSourceMute = toggleSourceMute


volChange :: Bool -> Int -> IO ()
volChange noLimit inc = do
    SinkState { .. } <- getDefaultSinkState
    let limit = if noLimit then 200 else 100
    let newVolume = min (_sinkVolume + inc) limit
    let newVolume' = max newVolume 0
    setSinkVolume _sinkName newVolume'
    notifyVolume newVolume' _sinkIsMuted


toggleMute :: IO ()
toggleMute = do
    SinkState { .. } <- getDefaultSinkState
    let newIsMuted = not _sinkIsMuted
    setSinkMute _sinkName newIsMuted
    notifyVolume _sinkVolume newIsMuted


toggleSourceMute :: IO ()
toggleSourceMute = do
    SourceState { .. } <- getDefaultSourceState
    setSourceMute _sourceName (not _sourceIsMuted)
