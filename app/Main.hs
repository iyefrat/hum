module Main where

import           Brick.Main
import qualified Brick.BChan                   as BC
import qualified Graphics.Vty                  as Vty
import           UI                             ( buildInitialState
                                                , app
                                                )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )
import           Hmm.Types
import           Network.MPD                    ( withMPD
                                                , idle
                                                )

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

mpdListenForever :: BC.BChan HmmEvent -> IO ()
mpdListenForever chan = forever $ do
  response <- withMPD $ idle []
  BC.writeBChan chan (Right response)

tickTock :: BC.BChan HmmEvent -> IO ()
tickTock chan = forever $ do
  BC.writeBChan chan (Left Tick)
  threadDelay 1000000
