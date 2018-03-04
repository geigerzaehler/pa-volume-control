{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM
import Control.Monad.Trans.Resource as Res

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML

import Test.Setup
import Test.Support


main :: IO ()
main = do
    defaultMainWithIngredients
        (antXMLRunner:defaultIngredients)
        tests

tests :: TestTree
tests = withResource' setup $ \c -> testGroup "cli"
    [ test_toggleMute c
    ]

withResource' :: forall a. ResIO a -> (IO a -> TestTree) -> TestTree
withResource' resource mkTest = withResource acquire close mkTest'
    where
        acquire :: IO (a, Res.InternalState)
        acquire = do
            resourceState <- Res.createInternalState
            a <- runInternalState resource resourceState
            return (a, resourceState)

        close :: (a, Res.InternalState) -> IO ()
        close (_, resourceState) = closeInternalState resourceState

        mkTest' :: IO (a, Res.InternalState) -> TestTree
        mkTest' get = mkTest (fst <$> get)


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
