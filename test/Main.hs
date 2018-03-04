{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads(..))
import Test.Tasty.Runners.AntXML

import Test.Setup
import Test.Support


main :: IO ()
main = do
    defaultMainWithIngredients
        (antXMLRunner:defaultIngredients)
        tests


tests :: TestTree
tests =
    -- We can only run one thread because there is only one
    -- notification server and one PA_STATE_FILE
    localOption (NumThreads 1) $
    withResource' setup $ \c -> testGroup "cli"
    [ test_toggleMute c
    , test_volumeControl c
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
