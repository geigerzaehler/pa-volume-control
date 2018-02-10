{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Monoid ((<>))

import VolumeControl.Command
import Options.Applicative


helpString :: String
helpString =
    "Usage:\n\
    \        pa-volume-control volup [--nolimit]\n\
    \        pa-volume-control voldown\n\
    \        pa-volume-control mutetoggle\n"


rootParser :: Parser Command
rootParser =
    (infoOption helpString $ short 'h' <> long "help")
    <*> subparser
        ( volumeUpCommand <> volumeDownCommand <> toggleMuteCommand)


volumeUpCommand :: Mod CommandFields Command
volumeUpCommand = command "volup" (info opts mempty)
    where
    opts = VolumeUp <$> flag False True (long "nolimit")

volumeDownCommand :: Mod CommandFields Command
volumeDownCommand = command "voldown" (info (pure VolumeDown) mempty)

toggleMuteCommand :: Mod CommandFields Command
toggleMuteCommand = command "mutetoggle" (info (pure ToggleMute) mempty)

main :: IO ()
main = do
    cmd <- execParser $ info rootParser mempty
    execCommand cmd
