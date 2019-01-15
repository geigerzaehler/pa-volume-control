{-# LANGUAGE ScopedTypeVariables #-}
module Test.Support (
    withResource'
  , writePacmdState
  , runVolumeControl
  , runPacmd
  , assertNotificationMessage
  , assertStateLine
) where


import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Resource as Res

import           Data.Foldable
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Exit
import System.Environment
import System.Process
import Text.Printf

import Test.Setup
import Test.Tasty
import Test.Tasty.HUnit


-- | Like 'Test.Tasty.withResource' but using the resource monad
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



writePacmdState :: [String] -> IO ()
writePacmdState contents = do
    filePath <- getEnv "PACMD_STATE_FILE"
    T.writeFile filePath $ T.pack $ unlines contents


assertNotificationMessage :: (HasCallStack) => TVar Notification -> String -> IO ()
assertNotificationMessage lastNotification expected = do
    (_, message) <- readTVarIO lastNotification
    message @?= expected


-- | Assert that the given string is present in the pulseaudio mock
-- state.
assertStateLine :: (HasCallStack) => String -> IO ()
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

runPacmd :: [String] -> IO ()
runPacmd args = void $ readProcess "pacmd" args []
