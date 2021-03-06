{-# LANGUAGE OverloadedStrings #-}

module VolumeControl.Main (main) where

import Data.Monoid ((<>))
import VolumeControl.Command
import Options.Applicative

main :: IO ()
main = do
    cmd <- execParser $ info commandParser mempty
    execCommand cmd

helpString :: String
helpString =
    "Change volume through PulseAudio\n\n\
    \Usage:\n\
    \        pa-volume-control volup\n\
    \        pa-volume-control voldown\n\
    \        pa-volume-control mutetoggle\n\
    \        pa-volume-control source-mute-toggle\n"


commandParser :: Parser Command
commandParser =
    infoOption helpString (short 'h' <> long "help")
    <*> subparser
        ( volumeUpCommand <> volumeDownCommand <> toggleMuteCommand <> toggleSourceMuteCommand)


volumeUpCommand :: Mod CommandFields Command
volumeUpCommand = command "volup" (info opts mempty)
    where
    opts = VolumeUp <$> flag False True (long "nolimit")

volumeDownCommand :: Mod CommandFields Command
volumeDownCommand = command "voldown" (info (pure VolumeDown) mempty)

toggleMuteCommand :: Mod CommandFields Command
toggleMuteCommand = command "mutetoggle" (info (pure ToggleMute) mempty)

toggleSourceMuteCommand :: Mod CommandFields Command
toggleSourceMuteCommand = command "source-mute-toggle" (info (pure ToggleSourceMute) mempty)
