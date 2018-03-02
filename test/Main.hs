{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import           Data.Foldable
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Exit
import System.Environment
import System.Process
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML

import Test.Setup


main :: IO ()
main = runResourceT $ do
    lastNotification <- setup
    liftIO $ do
        defaultMainWithIngredients
            (antXMLRunner:defaultIngredients)
            (test_toggleMute lastNotification)




test_toggleMute :: TVar Notification -> TestTree
test_toggleMute lastNotification = testCaseSteps "toggle mute" $ \step -> do
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

    _ <- readProcess "pacmd" ["set-default-sink", "OTHER_SINK"] []

    step "Other sink unmute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK no"
    assertStateLine "set-sink-mute OTHER_SINK no"

    step "Other sink mute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK no"
    assertStateLine "set-sink-mute OTHER_SINK yes"

--
-- * Helpers
--

writePacmdState :: [String] -> IO ()
writePacmdState contents = do
    filePath <- getEnv "PACMD_STATE_FILE"
    T.writeFile filePath $ T.pack $ unlines contents


assertNotificationMessage :: TVar Notification -> String -> IO ()
assertNotificationMessage lastNotification expected = do
    (_, message) <- (atomically $ readTVar lastNotification)
    message @=? expected


-- | Assert that the given string is present in the pulseaudio mock
-- state.
assertStateLine :: String -> IO ()
assertStateLine line = do
    let errorTemplate =
            "Could not find line in pulse audio state\n\
            \-- EXPECTED -----\n\
            \%s\n\
            \-- ACTUAL -------\n\
            \%s\
            \-----------------"
    filePath <- getEnv "PACMD_STATE_FILE"
    contents <- T.unpack <$> T.readFile filePath
    let found = find (== line) (lines contents)
    when (isNothing found) $
        assertFailure $ printf errorTemplate line contents


-- | Run 'pa-volume-control' with the given arguments and asserts that
-- the command exits sucesfully.
runVolumeControl :: [String] -> IO ()
runVolumeControl args = do
    let errorTemplate =
            "pa-volume-control exited with code %d\n\
            \-- STDERR -------\n\
            \%s\
            \-----------------"
    (exitCode, _stdout, stderr') <- readProcessWithExitCode "pa-volume-control" args []
    case exitCode of
        ExitFailure code ->
            assertFailure $ printf errorTemplate code stderr'
        _ -> return ()
