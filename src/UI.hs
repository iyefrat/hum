module UI where


import           Brick.Main
import           Brick.Types
import qualified Brick.BChan                   as BC
import           Brick.Widgets.Core
import           Brick.Widgets.List
import           Graphics.Vty.Input.Events
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Ham.Types
import qualified Data.Vector                   as V
import           Data.Time                      ( getCurrentTime )
import           Ham.Attributes
import           Ham.Views
import           Ham.Utils
import qualified Data.Map.Strict               as Map

app :: App HState HamEvent Name

app = App { appDraw         = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = hamStartEvent
          , appAttrMap      = const hamAttrMap
          }

drawUI :: HState -> [Widget Name]
drawUI st =
  [ drawNowPlaying st
      <=> (case view st of
            QueueView     -> drawViewQueue st
            LibraryView   -> drawViewLibrary st
            PlaylistsView -> drawViewPlaylists st
          )
  ]

buildInitialState :: BC.BChan HamEvent -> IO HState
buildInitialState chan = do
  let mode = NormalMode
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  queueVec <- V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue = (, False) <$> list QueueList queueVec 1
  currentTime <- getCurrentTime
  artistsVec  <- V.fromList . fromRight [] <$> withMPD
    (MPD.list MPD.AlbumArtist Nothing)
  let artists = list ArtistsList artistsVec 1
  albumsVec <- albumsOfArtist (snd <$> listSelectedElement artists)
  let albums    = list AlbumsList albumsVec 1
  let view      = QueueView
  let extentMap = Map.empty
  let clipboard = list Clipboard V.empty 1
  songsVec <- songsOfAlbum (snd <$> listSelectedElement albums)
  let songs = list SongsList songsVec 1
  let focus = Focus { focQueue = FocQueue
                    , focLib   = FocArtists
                    , focPlay  = FocPlaylists
                    }
  playlistsVec <- V.fromList . fromRight [] <$> withMPD (MPD.listPlaylists)
  let playlists = list PlaylistList playlistsVec 1
  playlistSongsVec <- V.fromList . fromRight [] <$> withMPD
    (MPD.listPlaylistInfo
      (fromMaybe "<no playlists>" $ snd <$> listSelectedElement playlists)
    )
  let playlistSongs = list PlaylistSongs playlistSongsVec 1
  pure HState { chan
              , view
              , mode
              , status
              , currentSong
              , queue
              , extentMap
              , clipboard
              , currentTime
              , artists
              , songs
              , focus
              , albums
              , playlists
              , playlistSongs
              }

hamStartEvent :: HState -> EventM Name HState
hamStartEvent s = do
  pure s

hBoxPad :: Padding -> [Widget n] -> Widget n
hBoxPad _ []       = emptyWidget
hBoxPad _ [w     ] = w
hBoxPad p (w : ws) = padRight p w <+> hBoxPad p ws

seekCurEventM :: MPD.FractionalSeconds -> HState -> EventM Name (Next HState)
seekCurEventM i s = do
  _      <- liftIO (withMPD $ MPD.seekCur False i)
  status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
  song   <- liftIO (withMPD MPD.currentSong)
  continue s { currentSong = fromRight Nothing song, status }

handleEvent :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey (KChar 't') [] -> do
      st <- liftIO ((MPD.stState <$>) <$> withMPD MPD.status)
      _  <- case st of
        Left  _           -> liftIO (withMPD $ MPD.pause True)
        Right MPD.Paused  -> liftIO (withMPD $ MPD.play Nothing)
        Right MPD.Stopped -> liftIO (withMPD $ MPD.play Nothing)
        Right MPD.Playing -> liftIO (withMPD $ MPD.pause True)
      continue s
    EvKey (KChar 'm') [] -> error "show options for modes"
    EvKey (KChar '.') [] -> do
      _    <- liftIO (withMPD MPD.next)
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar ',') [] -> do
      _    <- liftIO (withMPD MPD.previous)
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar ']') [] -> seekCurEventM 5 s
    EvKey (KChar '[') [] -> seekCurEventM (-5) s
    EvKey (KChar '}') [] -> seekCurEventM 30 s
    EvKey (KChar '{') [] -> seekCurEventM (-30) s
    EvKey (KChar '1') [] -> do
      _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
      continue s { view = QueueView }
    EvKey (KChar '2') [] -> do
      _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
      continue s { view = LibraryView }
    EvKey (KChar '3') [] -> do
      _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
      continue s { view = PlaylistsView }
    EvResize _ _ -> do
      extentMap <- updateExtentMap
      continue s { extentMap }
    _ -> case view s of
      QueueView     -> handleEventQueue s e
      LibraryView   -> handleEventLibrary s e
      PlaylistsView -> handleEventPlaylists s e
  (AppEvent (Left Tick)) -> do
    extentMap <- updateExtentMap
    status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    currentTime <- liftIO getCurrentTime
    continue s { currentTime, status, extentMap }
  (AppEvent (Right (Right _))) -> do
    currentSong <- liftIO (fromRight Nothing <$> withMPD MPD.currentSong)
    status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    queueVec <- liftIO
      (V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing))
    let queueUnmoved = (, False) <$> list QueueList queueVec 1
    let queueNew = case listSelected (queue s) of
          Nothing -> queueUnmoved
          Just i  -> listMoveTo i queueUnmoved
    continue s { currentSong, status, queue = queueNew }
  _ -> continue s

--handleEvent s (VtyEvent e) = continue =<< handleListEventVi handleListEvent e s

{-
TODO write generic Response handler to pring the MPDError instead of doing the thing.
-----
TODO update visuals
TODO go over entire project and tidy up
TODO search!
TODO playlist editing
TODO go over HState, nest? lenses?
TODO Stop and lint everything
TODO Various Artists
TODO hackage?
TODO more vim motions
TODO random song order (and friends)
TODO (oneday) album art stuff, look into hip and jucypixels on hackage
-}
