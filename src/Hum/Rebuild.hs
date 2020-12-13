-- |

module Hum.Rebuild where

import           Hum.Types
import           Control.Lens
import           Brick.Widgets.List
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Vector                   as V
-- in which we have funcitons to rebuild the state when it changes.

songsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfArtist martist = V.fromList . fromRight [] <$> withMPD
  (MPD.find (MPD.AlbumArtist MPD.=? fromMaybe "" martist))

songsOfAlbum :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfAlbum malbum = V.fromList . fromRight [] <$> withMPD
  (MPD.find (MPD.Album MPD.=? fromMaybe "" malbum))

albumsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Value)
albumsOfArtist martist =
  V.fromList . fromRight [] <$> withMPD (MPD.list MPD.Album martist)


rebuildLib :: MonadIO m => HState -> m HState
rebuildLib s = do
    artistsVec <- liftIO (V.fromList . fromRight [] <$> withMPD
      (MPD.list MPD.AlbumArtist Nothing))
    let artists' = list ArtistsList artistsVec 1
    albumsVec   <- liftIO $ albumsOfArtist (snd <$> listSelectedElement artists')
    let albums'  = list AlbumsList albumsVec 1
    songsVec    <- liftIO $ songsOfAlbum (snd <$> listSelectedElement albums')
    let songs'   = list SongsList songsVec 1
    pure $ s &  libraryL . artistsL .~ artists'
             &  libraryL . albumsL .~ albums'
             &  libraryL . songsL .~ songs'

rebuildLibArtists :: MonadIO m => HState -> m HState
rebuildLibArtists s = do
    let artists' = s ^. libraryL . artistsL
    albumsVec   <- liftIO $ albumsOfArtist (snd <$> listSelectedElement artists')
    let albums'  = list AlbumsList albumsVec 1
    songsVec    <- liftIO $ songsOfAlbum (snd <$> listSelectedElement albums')
    let songs'   = list SongsList songsVec 1
    pure $ s &  libraryL . artistsL .~ artists'
             &  libraryL . albumsL .~ albums'
             &  libraryL . songsL .~ songs'

rebuildLibAlbums :: MonadIO m => HState -> m HState
rebuildLibAlbums s = do
    let albums' = s ^. libraryL . albumsL
    songsVec   <- liftIO $ songsOfAlbum (snd <$> listSelectedElement albums')
    let songs'  = list SongsList songsVec 1
    pure $ s & libraryL . albumsL .~ albums' & libraryL . songsL .~ songs'

rebuildPl :: MonadIO m => HState -> m HState
rebuildPl s = do
  plListVec  <- liftIO $  V.fromList . sort . fromRight [] <$> withMPD MPD.listPlaylists
  let plList' = list PlaylistList plListVec 1
  plSongsVec <- liftIO $ V.fromList . fromRight [] <$> withMPD
           (MPD.listPlaylistInfo
               (maybe "<no playlists>" snd (listSelectedElement plList'))
           )
  let plSongs' = (, False) <$> list PlaylistSongs plSongsVec 1
  pure $ s & playlistsL . plListL  .~ plList'
           & playlistsL . plSongsL .~ plSongs'

rebuildPlList :: MonadIO m => HState -> m HState
rebuildPlList s = do
    let plList' = s ^. playlistsL . plListL
    plSongsVec <- liftIO
        (V.fromList . fromRight [] <$> withMPD
            (MPD.listPlaylistInfo
                (maybe "<no playlists>" snd (listSelectedElement plList'))
            )
        )
    let plSongs' = (, False) <$> list PlaylistSongs plSongsVec 1
    pure $ s & playlistsL . plListL  .~ plList'
             & playlistsL . plSongsL .~ plSongs'

rebuildQueue :: MonadIO m => HState -> m HState
rebuildQueue s = do
  let mi = listSelected (queue s)
  queueVec  <- liftIO $ V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue' = (, False) <$> list QueueList queueVec 1
  pure $ s & queueL .~ maybe id listMoveTo mi queue'

rebuildStatus :: MonadIO m => HState -> m HState
rebuildStatus s = do
  currentSong' <- liftIO (fromRight Nothing <$> withMPD MPD.currentSong)
  status'      <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
  pure $ s & currentSongL .~ currentSong'
           & statusL .~ status'
