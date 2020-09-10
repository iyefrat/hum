-- |

module Ham.Utils where
import           Ham.Types
import           Data.Vector                   as V
import           Brick.Widgets.List
import           Data.Time                      ( getCurrentTime )
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

songsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfArtist martist =
  (   V.fromList
  <$> fromRight []
  <$> (withMPD $ MPD.find (MPD.AlbumArtist MPD.=? fromMaybe "" martist))
  )

songsOfAlbum :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfAlbum malbum =
  (   V.fromList
  <$> fromRight []
  <$> (withMPD $ MPD.find (MPD.Album MPD.=? fromMaybe "" malbum))
  )
albumsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Value)
albumsOfArtist martist =
  (V.fromList <$> fromRight [] <$> (withMPD $ MPD.list MPD.Album martist))
