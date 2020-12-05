-- |

module Hum.Rebuild where

import           Hum.Types
import           Hum.Utils
import           Lens.Micro
import           Brick.Widgets.List
import           Brick.Types
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Vector                   as V
-- in which we have funcitons to rebuild the state when it changes.


rebuildLib :: HState -> EventM n HState
rebuildLib s = do
    artistsVec <- liftIO (V.fromList . fromRight [] <$> withMPD
      (MPD.list MPD.AlbumArtist Nothing))
    let artists' = list ArtistsList artistsVec 1
    albumsVec <- liftIO (albumsOfArtist (snd <$> listSelectedElement artists'))
    let albums' = list AlbumsList albumsVec 1
    songsVec <- liftIO (songsOfAlbum (snd <$> listSelectedElement albums'))
    let songs' = list SongsList songsVec 1
    pure $ s &  libraryL . artistsL .~ artists'
             &  libraryL . albumsL .~ albums'
             &  libraryL . songsL .~ songs'

rebuildLibArtists :: HState -> EventM n HState
rebuildLibArtists s = do
    let artists' = s ^. libraryL . artistsL
    albumsVec <- liftIO (albumsOfArtist (snd <$> listSelectedElement artists'))
    let albums' = list AlbumsList albumsVec 1
    songsVec <- liftIO (songsOfAlbum (snd <$> listSelectedElement albums'))
    let songs' = list SongsList songsVec 1
    pure $ s &  libraryL . artistsL .~ artists'
             &  libraryL . albumsL .~ albums'
             &  libraryL . songsL .~ songs'

rebuildLibAlbums :: HState -> EventM n HState
rebuildLibAlbums s = do
    let albums' = s ^. libraryL . albumsL
    songsVec <- liftIO (songsOfAlbum (snd <$> listSelectedElement albums'))
    let songs' = list SongsList songsVec 1
    pure $ s & libraryL . albumsL .~ albums' & libraryL . songsL .~ songs'

rebuildPl :: HState -> EventM n HState
rebuildPl s = do
  plListVec <- liftIO (V.fromList . fromRight [] <$> withMPD MPD.listPlaylists)
  let plList' = list PlaylistList plListVec 1
  plSongsVec <- liftIO
       (V.fromList . fromRight [] <$> withMPD
           (MPD.listPlaylistInfo
               (maybe "<no playlists>" snd (listSelectedElement plList'))
           )
       )
  let plSongs' = (, False) <$> list PlaylistSongs plSongsVec 1
  pure $ s & playlistsL . plListL .~ plList'
            & playlistsL . plSongsL.~ plSongs'

rebuildPlList :: HState -> EventM n HState
rebuildPlList s = do
    let plList' = s ^. playlistsL . plListL
    plSongsVec <- liftIO
        (V.fromList . fromRight [] <$> withMPD
            (MPD.listPlaylistInfo
                (maybe "<no playlists>" snd (listSelectedElement plList'))
            )
        )
    let plSongs' = (, False) <$> list PlaylistSongs plSongsVec 1
    pure $ s & playlistsL . plListL .~ plList'
             & playlistsL . plSongsL.~ plSongs'
