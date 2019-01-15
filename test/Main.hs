{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent.STM

import System.Environment

import Test.Tasty
import Test.Tasty.HUnit

import Test.Setup
import Test.Support


main :: IO ()
main = do
    -- We can only run one thread because there is only one
    -- notification server and one PA_STATE_FILE
    setEnv "TASTY_NUM_THREADS" "1"
    defaultMain tests


tests :: TestTree
tests =
    withResource' setup $ \c -> testGroup "cli"
    [ test_toggleMute c
    , test_toggleSourceMute
    , test_volumeControl c
    , test_volumeControlMinimum c
    , test_volumeControlMaximum c
    ]


test_toggleMute :: IO (TVar Notification) -> TestTree
test_toggleMute getLastNotification = testCaseSteps "toggle mute" $ \step -> do
    lastNotification <- getLastNotification
    writePacmdState
        [ "set-default-sink DEFAULT_SINK"
        , "set-sink-volume DEFAULT_SINK 0x8000"
        , "set-sink-mute DEFAULT_SINK no"
        , "set-sink-volume OTHER_SINK 0x8000"
        , "set-sink-mute OTHER_SINK yes"
        ]

    step "Default sink mute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK yes"
    assertStateLine "set-sink-mute OTHER_SINK yes"
    assertNotificationMessage lastNotification "Muted 50%"


    step "Default sink unmute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK no"
    assertStateLine "set-sink-mute OTHER_SINK yes"
    assertNotificationMessage lastNotification "Volume 50%"

    runPacmd ["set-default-sink", "OTHER_SINK"]

    step "Other sink unmute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK no"
    assertStateLine "set-sink-mute OTHER_SINK no"

    step "Other sink mute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK no"
    assertStateLine "set-sink-mute OTHER_SINK yes"

test_toggleSourceMute :: TestTree
test_toggleSourceMute = testCaseSteps "toggle source mute" $ \step -> do
    writePacmdState
        [ "set-default-source DEFAULT_SOURCE"
        , "set-source-mute DEFAULT_SOURCE no"
        , "set-source-mute OTHER_SOURCE yes"
        ]

    step "Default source mute"
    assertStateLine "set-source-mute DEFAULT_SOURCE no"
    assertStateLine "set-source-mute OTHER_SOURCE yes"
    runVolumeControl ["source-mute-toggle"]
    assertStateLine "set-source-mute DEFAULT_SOURCE yes"
    assertStateLine "set-source-mute OTHER_SOURCE yes"


    step "Default source unmute"
    runVolumeControl ["source-mute-toggle"]
    assertStateLine "set-source-mute DEFAULT_SOURCE no"
    assertStateLine "set-source-mute OTHER_SOURCE yes"

    runPacmd ["set-default-source", "OTHER_SOURCE"]

    step "Other source unmute"
    runVolumeControl ["source-mute-toggle"]
    assertStateLine "set-source-mute DEFAULT_SOURCE no"
    assertStateLine "set-source-mute OTHER_SOURCE no"

    step "Other source mute"
    runVolumeControl ["source-mute-toggle"]
    assertStateLine "set-source-mute DEFAULT_SOURCE no"
    assertStateLine "set-source-mute OTHER_SOURCE yes"

test_volumeControl :: IO (TVar Notification) -> TestTree
test_volumeControl getLastNotification = testCaseSteps "volume control" $ \step -> do
    lastNotification <- getLastNotification
    writePacmdState
        [ "set-default-sink DEFAULT_SINK"
        , "set-sink-volume DEFAULT_SINK 0x8000"
        , "set-sink-mute DEFAULT_SINK no"
        , "set-sink-volume OTHER_SINK 0x8000"
        , "set-sink-mute OTHER_SINK yes"
        ]

    step "Default sink volume up"
    runVolumeControl ["volup"]
    assertStateLine "set-sink-volume DEFAULT_SINK 0x999a"
    assertStateLine "set-sink-volume OTHER_SINK 0x8000"
    assertNotificationMessage lastNotification "Volume 60%"

    step "Default sink volume down"
    runVolumeControl ["voldown"]
    assertStateLine "set-sink-volume DEFAULT_SINK 0x8000"
    assertStateLine "set-sink-volume OTHER_SINK 0x8000"
    assertNotificationMessage lastNotification "Volume 50%"

    runPacmd ["set-default-sink", "OTHER_SINK"]

    step "Other sink volume down"
    runVolumeControl ["voldown"]
    assertStateLine "set-sink-volume DEFAULT_SINK 0x8000"
    assertStateLine "set-sink-volume OTHER_SINK 0x6666"
    assertNotificationMessage lastNotification "Muted 40%"


test_volumeControlMinimum :: IO (TVar Notification) -> TestTree
test_volumeControlMinimum getLastNotification = testCase "volume control minimum" $ do
    lastNotification <- getLastNotification
    writePacmdState
        [ "set-default-sink DEFAULT_SINK"
        , "set-sink-volume DEFAULT_SINK 0x0001"
        , "set-sink-mute DEFAULT_SINK no"
        ]

    runVolumeControl ["voldown"]
    assertStateLine "set-sink-volume DEFAULT_SINK 0x0"
    assertNotificationMessage lastNotification "Volume 0%"


test_volumeControlMaximum :: IO (TVar Notification) -> TestTree
test_volumeControlMaximum getLastNotification = testCaseSteps "volume control maximum" $ \step -> do
    step "Limit volume"
    lastNotification <- getLastNotification
    writePacmdState
        [ "set-default-sink DEFAULT_SINK"
        , "set-sink-volume DEFAULT_SINK 0xffff"
        , "set-sink-mute DEFAULT_SINK no"
        ]

    runVolumeControl ["volup"]
    assertStateLine "set-sink-volume DEFAULT_SINK 0x10000"
    assertNotificationMessage lastNotification "Volume 100%"

    runVolumeControl ["volup"]
    assertStateLine "set-sink-volume DEFAULT_SINK 0x10000"
    assertNotificationMessage lastNotification "Volume 100%"

    step "Beyond limit"
    runVolumeControl ["volup", "--nolimit"]
    assertStateLine "set-sink-volume DEFAULT_SINK 0x1199a"
    assertNotificationMessage lastNotification "Volume 110%"
