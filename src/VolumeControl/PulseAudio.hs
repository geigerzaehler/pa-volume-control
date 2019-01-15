{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module VolumeControl.PulseAudio (
    getDefaultSinkState
  , setSinkVolume
  , setSinkMute
  , SinkState(..)
  , Volume
) where


import           Data.Foldable
import           Data.Int (Int32)
import           Data.String (lines, words)
import           Numeric
import           Text.Read (readMaybe)
import           System.Process


-- | Volume as a percentage
type Volume = Int


--
-- Volume control
--
setSinkVolume :: String -> Volume -> IO ()
setSinkVolume sink volume = do
    let volume' = max volume 0
    let volString = "0x" <> showHex (volumeToRaw volume') ""
    _ <- readProcessWithExitCode "pacmd" ["set-sink-volume", sink, volString] ""
    return ()


setSinkMute :: String -> Bool -> IO ()
setSinkMute sinkName isMute = do
    let muteString = if isMute then "yes" else "no"
    _ <- readProcessWithExitCode "pacmd" ["set-sink-mute", sinkName, muteString] ""
    return ()


data SinkState = SinkState
    { _sinkVolume :: Int
    , _sinkIsMuted :: Bool
    , _sinkName :: String }


-- | Get the sink state for the default sink from the @pacmd@ command.
getDefaultSinkState :: IO SinkState
getDefaultSinkState = do
    infoDump <- getDump
    let Just sinkName = readDefaultSink infoDump
    let Just sinkIsMuted = readSinkMute sinkName infoDump
    let Just sinkVolume = readSinkVolume sinkName infoDump
    return SinkState
        { _sinkName = sinkName
        , _sinkVolume = sinkVolume
        , _sinkIsMuted = sinkIsMuted }


readDefaultSink :: PulseAudioDump -> Maybe String
readDefaultSink = dumpLookup1 "set-default-sink" stringParser

readSinkMute :: String -> PulseAudioDump -> Maybe Bool
readSinkMute sinkName =
    dumpLookup2 "set-sink-mute" sinkName boolParser

readSinkVolume :: String -> PulseAudioDump -> Maybe Volume
readSinkVolume sinkName =
    dumpLookup2 "set-sink-volume" sinkName volumeParser

--
-- * Dump handling
--

newtype PulseAudioDump = PulseAudioDump [[String]]

getDump :: IO PulseAudioDump
getDump = PulseAudioDump . parse <$> readProcess "pacmd" ["dump"] []
  where
    parse = map words . filter isNotComment . lines
    isNotComment line = take 1 line /= "#"

dumpLookup1 :: String -> Parser a -> PulseAudioDump -> Maybe a
dumpLookup1 x1 parse (PulseAudioDump dump) = asum $ map match dump
  where
    match [x1', value] | x1' == x1 = parse value
    match _ = Nothing

dumpLookup2 :: String -> String -> Parser a -> PulseAudioDump -> Maybe a
dumpLookup2 x1 x2 parse (PulseAudioDump dump) = asum $ map match dump
  where
    match [x1', x2', value] | x1' == x1, x2' == x2 = parse value
    match _ = Nothing

type Parser a = String -> Maybe a

boolParser :: Parser Bool
boolParser "yes" = Just True
boolParser "no" = Just False
boolParser _ = Nothing

volumeParser :: Parser Volume
volumeParser value =  volumeFromRaw <$> readMaybe value

stringParser :: Parser String
stringParser = Just


--
-- Volumes
--


maxAbsoluteVolume :: Int32
maxAbsoluteVolume = 0x10000

volumeFromRaw :: Int32 -> Volume
volumeFromRaw volume = round $ 100 * toRational volume / toRational maxAbsoluteVolume

volumeToRaw :: Volume -> Int32
volumeToRaw percentage = round $ toRational percentage / 100 * toRational maxAbsoluteVolume
