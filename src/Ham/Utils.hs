-- |

module Ham.Utils where
import           Ham.Types
import           Data.Vector                   as V
import           Brick.Widgets.List
import           Data.Time                      ( getCurrentTime )
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD

rebuildState :: IO HState
rebuildState = do
  currentSong  <- fromRight Nothing <$> withMPD MPD.currentSong
  status       <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  playlist     <- fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  currentTime  <- getCurrentTime
  albumArtists <-
    fromRight [] <$> (withMPD $ MPD.list MPD.AlbumArtistSort Nothing)
  let view        = QueueView
  let queue = (, False) <$> list QueueList (V.fromList playlist) 1
  let queueExtent = Nothing
  let clipboard   = list Clipboard V.empty 1
  pure HState { view
              , status
              , currentSong
              , playlist
              , queue
              , queueExtent
              , clipboard
              , currentTime
              , albumArtists
              }
