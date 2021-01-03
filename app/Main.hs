
module Main where

import           Brick.Main
import qualified Brick.BChan                   as BC
import qualified Graphics.Vty                  as Vty
import           Hum.UI                         ( buildInitialState
                                                , app
                                                )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Hum.Types
import           Network.MPD                    ( withMPD
                                                , idle
                                                )
-- | Calls functions from "Hum.UI", go there next.
main :: IO ()
main = do
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
