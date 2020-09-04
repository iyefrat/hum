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
import           Types
--import           BChan                          ( mpdListenForever )
import           Network.MPD                    ( withMPD
                                                , idle
                                                )

main :: IO ()
main = do
  chan <- BC.newBChan 10
  forkIO $ mpdListenForever chan
  forkIO $ tickTock chan
  initialState <- buildInitialState
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just chan) app initialState
  pass

mpdListenForever :: BC.BChan HamEvent -> IO ()
mpdListenForever chan = forever $ do
  response <- withMPD $ idle []
  BC.writeBChan chan (Right response)

tickTock :: BC.BChan HamEvent -> IO ()
tickTock chan = forever $ do
  BC.writeBChan chan (Left Tick)
  threadDelay 100000
