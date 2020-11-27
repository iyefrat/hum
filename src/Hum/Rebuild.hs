-- |

module Hum.Rebuild where

import           Hum.Types
import           Hum.Utils
import           Lens.Micro
import           Brick.Widgets.List
import           Brick.Main
import           Brick.Types
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
-- in which we have funcitons to rebuild the state when it changes.


rebuildLibArtists :: HState  -> EventM Name (Next HState)
rebuildLibArtists s = do
        let artists' = s ^. libraryL . artistsL
        albumsVec <- liftIO
          (albumsOfArtist (snd <$> listSelectedElement artists'))
        let albums' = list AlbumsList albumsVec 1
        songsVec <- liftIO (songsOfAlbum (snd <$> listSelectedElement albums'))
        let songs' = list SongsList songsVec 1
        continue $ s & libraryL . artistsL .~ artists'
                     & libraryL . albumsL .~ albums'
                     & libraryL . songsL .~ songs'

rebuildLibAlbums :: HState -> EventM Name (Next HState)
rebuildLibAlbums s = do
        let albums' = s ^. libraryL . albumsL
        songsVec <- liftIO (songsOfAlbum (snd <$> listSelectedElement albums'))
        let songs' = list SongsList songsVec 1
        continue $ s & libraryL . albumsL .~ albums'
                     & libraryL . songsL .~ songs'
