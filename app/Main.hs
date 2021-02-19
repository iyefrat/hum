
-- | Module    : Main
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- The necessary logic to launch the hum executable, including commandline
-- arguments.

module Main where

import qualified Brick.BChan                   as BC
import           Brick.Main
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import qualified Graphics.Vty                  as Vty
import           Hum.Types               hiding ( help )
import           Hum.UI                         ( app
                                                , buildInitialState
                                                )
import           Network.MPD                    ( idle
                                                , withMPD
                                                )
import           Options.Applicative

-- | Calls functions from "Hum.UI", go there next.
main :: IO ()
main = do
  arg <- execParser opts
  case arg of
    HumArgs { version = True } -> putStrLn "hum 0.2.0.0"
    _                       -> do
      chan         <- BC.newBChan 10
      _            <- forkIO $ mpdListenForever chan
      _            <- forkIO $ tickTock chan
      initialState <- buildInitialState chan
      let buildVty = Vty.mkVty Vty.defaultConfig
      initialVty <- buildVty
      _          <- customMain initialVty buildVty (Just chan) app initialState
      pass

-- | Channel that waits for mpd events.
mpdListenForever :: BC.BChan HumEvent -> IO ()
mpdListenForever chan = forever $ do
  response <- withMPD $ idle []
  BC.writeBChan chan (Right response)

-- | Channel that sends ticks every 0.1 seconds.
tickTock :: BC.BChan HumEvent -> IO ()
tickTock chan = forever $ do
  BC.writeBChan chan (Left Tick)
  threadDelay 1000000

data HumArgs = HumArgs
  { version :: Bool
  }

args :: Parser HumArgs
args = HumArgs <$> switch (long "version" <> short 'v' <> help "Hum's version")

opts :: ParserInfo HumArgs
opts = info (args <**> helper)
            (fullDesc {-<> progDesc "Print a greeting for TARGET"-}
                      <> header "Hum - a haskell music manager")
