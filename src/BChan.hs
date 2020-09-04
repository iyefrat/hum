-- |
module BChan where
import           Network.MPD
mpdListenForever :: IO ()
mpdListenForever = forever
  (do
    response <- withMPD $ (idle [])
    putStrLn (show response)
  )
