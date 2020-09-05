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
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status      <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  queueVec <- V.fromList <$> fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  currentTime <- getCurrentTime
  artistsVec  <-
    V.fromList
    <$> fromRight []
    <$> (withMPD $ MPD.list MPD.AlbumArtistSort Nothing)
  let view        = QueueView
  let queue = (, False) <$> list QueueList queueVec 1
  let queueExtent = Nothing
  let clipboard   = list Clipboard V.empty 1
  let artists     = list ArtistsList artistsVec 1
  let focus       = FocArtists
  pure HState { view
              , status
              , currentSong
              , queueVec
              , queue
              , queueExtent
              , clipboard
              , currentTime
              , artistsVec
              , artists
              , focus
              }
