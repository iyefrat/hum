-- |

module Hum.Rebuild where

import           Hum.Types
import           Control.Lens
import           Data.Foldable
import           Brick.Widgets.List
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Vector                   as V
-- in which we have funcitons to rebuild the state when it changes.

songsOfArtist :: MPD.Value -> IO (V.Vector MPD.Song)
songsOfArtist artist' =
  (V.fromList . fromRight [] <$>)
  . withMPD
  . MPD.find $ (MPD.AlbumArtist MPD.=? artist')


songsOfAlbum ::MPD.Value -> IO (V.Vector MPD.Song)
songsOfAlbum album' =
   (V.fromList . fromRight [] <$>)
  . withMPD
  . MPD.find $ (MPD.Album MPD.=? album')


albumsOfArtist :: MPD.Value -> IO (V.Vector MPD.Value)
albumsOfArtist artist' =
  V.fromList . fromRight [] <$> withMPD (MPD.list MPD.Album (MPD.AlbumArtist MPD.=? artist'))

yalbumsOfArtist ::  Bool -> MPD.Value -> IO (V.Vector (MPD.Value,MPD.Value))
yalbumsOfArtist bl artist' = let srt = (if bl then fst else snd)
  in do
  albums' <- fromRight [] <$> withMPD (MPD.list MPD.Album (MPD.AlbumArtist MPD.=? artist'))
  yalbums' <- liftIO . sequence $ (\x -> (,x) <$> yearOfAlbum x) <$> albums'
  pure $ V.fromList (sortBy (\x y -> compare (srt x) (srt y)) yalbums')

yearOfAlbum :: MPD.Value -> IO MPD.Value
yearOfAlbum album' = fromRight "????" <$> (minYear <<$>> withMPD (MPD.list MPD.Date (MPD.Album MPD.=? album')))
  where minYear :: [MPD.Value] -> MPD.Value
        minYear [] = "????"
        minYear vals = minimum vals

rebuildLib :: MonadIO m => HumState -> m HumState
rebuildLib s = do
    artistsVec <- liftIO (V.fromList . fromRight [] <$> withMPD
      (MPD.list MPD.AlbumArtist mempty))
    let artists' = list ArtistsList artistsVec 1
    albumsVec   <- liftIO $ maybe (pure empty) albumsOfArtist (snd <$> listSelectedElement artists')
    let albums'  = list AlbumsList albumsVec 1
    yalbumsVec   <- liftIO $ maybe (pure empty) (yalbumsOfArtist (s ^. libraryL . yalbumSortL)) (snd <$> listSelectedElement artists')
    let yalbums'    = list YalbumsList yalbumsVec 1
    songsVec     <- liftIO $ maybe (pure empty) songsOfAlbum (snd <$> listSelectedElement albums')
    let songs'   = list SongsList songsVec 1
    pure $ s &  libraryL . artistsL .~ artists'
             &  libraryL . yalbumsL .~ yalbums'
             &  libraryL . songsL .~ songs'

rebuildLibArtists :: MonadIO m => HumState -> m HumState
rebuildLibArtists s = do
    let artists' = s ^. libraryL . artistsL
    yalbumsVec   <- liftIO $ maybe (pure empty) (yalbumsOfArtist (s ^. libraryL . yalbumSortL)) (snd <$> listSelectedElement artists')
    let yalbums'    = list YalbumsList yalbumsVec 1
    songsVec    <- liftIO $ maybe (pure empty) songsOfAlbum (snd . snd <$> listSelectedElement yalbums')
    let songs'   = list SongsList songsVec 1
    pure $ s &  libraryL . yalbumsL .~ yalbums'
             &  libraryL . songsL .~ songs'

rebuildLibAlbums :: MonadIO m => HumState -> m HumState
rebuildLibAlbums s = do
    let yalbums' = s ^. libraryL . yalbumsL
    songsVec   <- liftIO $ maybe (pure empty) songsOfAlbum (snd . snd <$> listSelectedElement yalbums')
    let songs'  = list SongsList songsVec 1
    pure $ s & libraryL . songsL .~ songs'

rebuildPl :: MonadIO m => HumState -> m HumState
rebuildPl s = do
  let mi = s ^. playlistsL . plListL & listSelected
  plListVec  <- liftIO $  V.fromList . sort . fromRight [] <$> withMPD MPD.listPlaylists
  let plList' = maybe id listMoveTo mi $ list PlaylistList plListVec 1
  plSongsVec <- liftIO $ V.fromList . fromRight [] <$> withMPD
           (MPD.listPlaylistInfo
               (maybe "<no playlists>" snd (listSelectedElement plList'))
           )
  let plSongs' = (, False) <$> list PlaylistSongs plSongsVec 1
  pure $ s & playlistsL . plListL  .~ plList'
           & playlistsL . plSongsL .~ plSongs'

rebuildPlList :: MonadIO m => HumState -> m HumState
rebuildPlList s = do
    let plList' = s ^. playlistsL . plListL
    plSongsVec <- liftIO
        (V.fromList . fromRight [] <$> withMPD
            (MPD.listPlaylistInfo
                (maybe "<no playlists>" snd (listSelectedElement plList'))
            )
        )
    let plSongs' = (, False) <$> list PlaylistSongs plSongsVec 1
    pure $ s & playlistsL . plSongsL .~ plSongs'

rebuildQueue :: MonadIO m => HumState -> m HumState
rebuildQueue s = do
  let mi = s ^. queueL & listSelected
  queueVec  <- liftIO $ V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue' = maybe id listMoveTo mi $ (, False) <$> list QueueList queueVec 1
  pure $ s & queueL .~ queue'

rebuildStatus :: MonadIO m => HumState -> m HumState
rebuildStatus s = do
  currentSong' <- liftIO (fromRight Nothing <$> withMPD MPD.currentSong)
  status'      <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
  pure $ s & currentSongL .~ currentSong'
           & statusL .~ status'
