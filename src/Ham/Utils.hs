-- |

module Ham.Utils where
import           Ham.Types
import           Brick.Types
import           Brick.Main
import           Data.Vector                   as V
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map

songsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfArtist martist = V.fromList . fromRight [] <$> withMPD
  (MPD.find (MPD.AlbumArtist MPD.=? fromMaybe "" martist))

songsOfAlbum :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfAlbum malbum = V.fromList . fromRight [] <$> withMPD
  (MPD.find (MPD.Album MPD.=? fromMaybe "" malbum))
albumsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Value)
albumsOfArtist martist =
  V.fromList . fromRight [] <$> withMPD (MPD.list MPD.Album martist)

updateExtentMap :: EventM Name (Map Name (Maybe (Extent Name)))
updateExtentMap = do
  queueE      <- lookupExtent Queue
  nowPlayingE <- lookupExtent NowPlaying
  libLeftE    <- lookupExtent LibraryLeft
  libRightE   <- lookupExtent LibraryRight
  let extentMap = Map.fromList
        [ (Queue       , queueE)
        , (NowPlaying  , nowPlayingE)
        , (LibraryLeft , libLeftE)
        , (LibraryRight, libRightE)
        ]
  pure extentMap
