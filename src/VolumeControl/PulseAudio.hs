{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VolumeControl.PulseAudio (
    getDefaultSinkState
  , setSinkVolume
  , setSinkMute
  , SinkState(..)
  , getDefaultSourceState
  , setSourceMute
  , SourceState(..)
  , Volume
) where


import           Data.Foldable
import           Data.Int (Int32)
import           Numeric
import           System.Process
import           Text.Read (readMaybe)
import qualified Data.Text as T


-- | Volume as a percentage
type Volume = Int


--
-- Volume control
--
setSinkVolume :: T.Text -> Volume -> IO ()
setSinkVolume sink volume = do
    let volume' = max volume 0
    let volString = "0x" <> showHex (volumeToRaw volume') ""
    _ <- readProcessWithExitCode "pacmd" ["set-sink-volume", T.unpack sink, volString] ""
    return ()


setSinkMute :: T.Text -> Bool -> IO ()
setSinkMute sinkName isMute = do
    let muteString = if isMute then "yes" else "no"
    _ <- readProcessWithExitCode "pacmd" ["set-sink-mute", T.unpack sinkName, muteString] ""
    return ()

setSourceMute :: T.Text -> Bool -> IO ()
setSourceMute sourceName isMute = do
    let muteString = if isMute then "yes" else "no"
    _ <- readProcessWithExitCode "pacmd" ["set-source-mute", T.unpack sourceName, muteString] ""
    return ()


data SinkState = SinkState
    { _sinkVolume :: Int
    , _sinkIsMuted :: Bool
    , _sinkName :: T.Text
    }


-- | Get the sink state for the default sink from the @pacmd@ command.
getDefaultSinkState :: IO SinkState
getDefaultSinkState = do
    infoDump <- getDump
    case readSink infoDump of
        Just x -> pure x
        Nothing -> error "Cannot find default sink"
  where
    readSink :: PulseAudioDump -> Maybe SinkState
    readSink dump = do
        _sinkName <- dumpLookup1 "set-default-sink" textParser dump
        _sinkIsMuted <- dumpLookup2 "set-sink-mute" _sinkName boolParser dump
        _sinkVolume <- dumpLookup2 "set-sink-volume" _sinkName volumeParser dump
        pure $ SinkState {..}


data SourceState = SourceState
    { _sourceIsMuted :: Bool
    , _sourceName :: T.Text
    }


-- | Get the source state for the default source from the @pacmd@ command.
getDefaultSourceState :: IO SourceState
getDefaultSourceState = do
    infoDump <- getDump
    case readSink infoDump of
        Just x -> pure x
        Nothing -> error "Cannot find default source"
  where
    readSink :: PulseAudioDump -> Maybe SourceState
    readSink dump = do
        _sourceName <- dumpLookup1 "set-default-source" textParser dump
        _sourceIsMuted <- dumpLookup2 "set-source-mute" _sourceName boolParser dump
        pure $ SourceState {..}


--
-- * Dump handling
--

newtype PulseAudioDump = PulseAudioDump [[T.Text]]

getDump :: IO PulseAudioDump
getDump = PulseAudioDump . parse . T.pack <$> readProcess "pacmd" ["dump"] []
  where
    parse = map T.words . filter isNotComment . T.lines
    isNotComment line = T.take 1 line /= "#"

dumpLookup1 :: T.Text -> Parser a -> PulseAudioDump -> Maybe a
dumpLookup1 x1 parse (PulseAudioDump dump) = asum $ map match dump
  where
    match [x1', value] | x1' == x1 = parse value
    match _ = Nothing

dumpLookup2 :: T.Text -> T.Text -> Parser a -> PulseAudioDump -> Maybe a
dumpLookup2 x1 x2 parse (PulseAudioDump dump) = asum $ map match dump
  where
    match [x1', x2', value] | x1' == x1, x2' == x2 = parse value
    match _ = Nothing

type Parser a = T.Text -> Maybe a

boolParser :: Parser Bool
boolParser "yes" = Just True
boolParser "no" = Just False
boolParser _ = Nothing

volumeParser :: Parser Volume
volumeParser value = volumeFromRaw <$> readMaybe (T.unpack value)

textParser :: Parser T.Text
textParser = Just


--
-- Volumes
--


maxAbsoluteVolume :: Int32
maxAbsoluteVolume = 0x10000

volumeFromRaw :: Int32 -> Volume
volumeFromRaw volume = round $ 100 * toRational volume / toRational maxAbsoluteVolume

volumeToRaw :: Volume -> Int32
volumeToRaw percentage = round $ toRational percentage / 100 * toRational maxAbsoluteVolume
