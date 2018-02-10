{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import           Data.Foldable
import           Data.Maybe
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Word

import Text.Printf
import System.Environment
import System.FilePath
import System.Process
import System.Exit

import Test.Tasty
import Test.Tasty.HUnit

import Test.Setup


main :: IO ()
main = runResourceT $ do
    lastNotification <- setup
    liftIO $ do
        prepareSearchPath
        setEnv "PACMD_STATE_FILE" "pa-state~"
        defaultMain (test_toggleMute lastNotification)




test_toggleMute :: TVar (Word32, String) -> TestTree
test_toggleMute lastNotification = testCaseSteps "toggle mute" $ \step -> do
    writePacmdState
        [ "set-default-sink DEFAULT_SINK"
        , "set-sink-volume DEFAULT_SINK 0x8000"
        , "set-sink-mute DEFAULT_SINK no"
        ]

    step "Default sink mute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK yes"
    (_, message) <- (atomically $ readTVar lastNotification)
    putStrLn message

    step "Default sink unmute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute DEFAULT_SINK no"

    writePacmdState
        [ "set-default-sink OTHER_SINK"
        , "set-sink-volume DEFAULT_SINK 0x8000"
        , "set-sink-mute DEFAULT_SINK no"
        , "set-sink-volume OTHER_SINK 0x8000"
        , "set-sink-mute OTHER_SINK yes"
        ]

    -- TODO assert that the other sink is not toggled
    step "Other sink unmute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute OTHER_SINK no"

    step "Other sink mute"
    runVolumeControl ["mutetoggle"]
    assertStateLine "set-sink-mute OTHER_SINK yes"

--
-- * Helpers
--

writePacmdState :: [String] -> IO ()
writePacmdState contents = do
    T.writeFile "pa-state~" $ T.pack $ unlines contents


assertStateLine :: String -> IO ()
assertStateLine line = do
    let errorTemplate =
            "Could not find line in pulse audio state\n\
            \-- EXPECTED -----\n\
            \%s\n\
            \-- ACTUAL -------\n\
            \%s\
            \-----------------"
    contents <- T.unpack <$> T.readFile "pa-state~"
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


-- | Make sure that the local build of 'pa-volume-control' and the
-- 'pacmd' stub command are available from the search path.
prepareSearchPath :: IO ()
prepareSearchPath  = do
    cabalBuildDir <- getEnv "CABAL_BUILD_DIR"
    let additionalPaths =
            [ cabalBuildDir </> "pa-volume-control"
            , "./test/bin-stubs"
            ]
    searchPath <- getEnv "PATH"
    let newSearchPath = intercalate [ searchPathSeparator ] $ additionalPaths ++ [ searchPath ]
    setEnv "PATH" newSearchPath
