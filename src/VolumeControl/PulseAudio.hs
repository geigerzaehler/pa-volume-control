{-# LANGUAGE ViewPatterns #-}

module VolumeControl.PulseAudio (
    getDefaultSinkState
  , setSinkVolume
  , setSinkMute
  , SinkState(..)
) where


import           Data.Foldable
import           Data.Int (Int32)
import           Data.Monoid
import           Data.String (lines, words)
import           Numeric
import           Text.Read (readMaybe)
import           System.Process


-- | Volume as a percentage
type Volume = Int

data SinkState = SinkState
    { _sinkVolume :: Int
    , _sinkIsMuted :: Bool
    , _sinkName :: String }


type PulseAudioDump = [(String, [String])]


getDefaultSinkState :: IO SinkState
getDefaultSinkState = do
    infoDump <- getDump
    let Just sinkName = readDefaultSink infoDump
    let Just sinkIsMuted = readSinkMute sinkName infoDump
    let Just sinkVolume = readSinkVolume sinkName infoDump
    return $ SinkState
        { _sinkName = sinkName
        , _sinkVolume = sinkVolume
        , _sinkIsMuted = sinkIsMuted }


getDump :: IO PulseAudioDump
getDump = do
    rawDump <- readProcess "pacmd" ["dump"] []
    return $ foldr' parseLine [] (lines rawDump)
    where
    parseLine :: String -> [(String, [String])] -> [(String, [String])]
    parseLine line items
        | take 1 line == "#" = items
        | (key : value) <- (words line) = (key, value) : items
        | otherwise = items


readDefaultSink :: PulseAudioDump -> Maybe String
readDefaultSink = asum . fmap match
    where
    match ("set-default-sink", [ sinkName ]) = Just sinkName
    match _ = Nothing


--
-- Volumes
--


maxAbsoluteVolume :: Int32
maxAbsoluteVolume = 0x10000

volumeFromRaw :: Int32 -> Volume
volumeFromRaw volume = round $ 100 * (toRational volume) /  (toRational maxAbsoluteVolume)

volumeToRaw :: Volume -> Int32
volumeToRaw percentage = round $ (toRational percentage) / 100 * (toRational maxAbsoluteVolume)



--
-- Volume control
--

readSinkVolume :: String -> PulseAudioDump -> Maybe Volume
readSinkVolume sinkName = asum . fmap match
    where
    match x
        | ("set-sink-volume", [ (sinkName'), volume ]) <- x
        , sinkName' == sinkName
        = volumeFromRaw <$> readMaybe volume
        | otherwise = Nothing


setSinkVolume :: String -> Volume -> IO ()
setSinkVolume sink volume = do
    let volume' = max volume 0
    let volString = "0x" <> showHex (volumeToRaw volume') ""
    _ <- readProcessWithExitCode "pacmd" ["set-sink-volume", sink, volString] ""
    return ()


readSinkMute :: String -> PulseAudioDump -> Maybe Bool
readSinkMute sinkName = asum . fmap match
    where
    match x
        | ("set-sink-mute", [ (sinkName'), isMute ]) <- x
        , sinkName' == sinkName
        = readBool isMute
        | otherwise = Nothing
    readBool "yes" = Just True
    readBool "no" = Just False
    readBool _ = Nothing


setSinkMute :: String -> Bool -> IO ()
setSinkMute sinkName isMute = do
    let muteString = if isMute then "yes" else "no"
    _ <- readProcessWithExitCode "pacmd" ["set-sink-mute", sinkName, muteString] ""
    return ()
