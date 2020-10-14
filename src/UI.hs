{-# LANGUAGE LambdaCase #-}
module UI where


import           Brick.Main
import           Brick.Types
import qualified Brick.BChan                   as BC
import           Brick.Widgets.Core
import           Brick.Widgets.List
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Graphics.Vty.Input.Events
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Hmm.Types
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Hmm.Attributes
import           Hmm.Views
import           Hmm.Modes
import           Hmm.Utils
import qualified Data.Map.Strict               as Map
import           Lens.Micro                     ( (?~)
                                                , (^.)
                                                , (^?)
                                                , (.~)
                                                , (%~)
                                                , _2
                                                , _head
                                                , set
                                                )

app :: App HState HmmEvent Name

app = App { appDraw         = drawUI
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = hmmStartEvent
          , appAttrMap      = const hmmAttrMap
          }

drawUI :: HState -> [Widget Name]
drawUI st =
  [ case mode st of
    SongModeMode -> drawSongModeHelp
    _            -> emptyWidget
  , drawNowPlaying st
    <=> (case view st of
          QueueView     -> drawViewQueue st
          LibraryView   -> drawViewLibrary st
          PlaylistsView -> drawViewPlaylists st
        )
    <=> renderEditor (txt . T.unlines) (st ^. focusL . focSearchL) (search st)
    <=> (hCenter . txt $ show (st ^. focusL . focSearchL))
  ]

chooseCursor :: HState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor st ls = if st ^. focusL . focSearchL
  then find isCurrent ls
  else Nothing
  where isCurrent cl = cl ^. cursorLocationNameL == Just SearchEditor

buildInitialState :: BC.BChan HmmEvent -> IO HState
buildInitialState chan = do
  let mode          = NormalMode
  let search = editorText SearchEditor (Just 1) "/"
  let searchHistory = []
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
  let focus = Focus { focQueue  = FocQueue
                    , focLib    = FocArtists
                    , focPlay   = FocPlaylists
                    , focSearch = False
                    }
  playlistsVec <- V.fromList . fromRight [] <$> withMPD MPD.listPlaylists
  let playlists = list PlaylistList playlistsVec 1
  playlistSongsVec <- V.fromList . fromRight [] <$> withMPD
    ( MPD.listPlaylistInfo
    $ maybe "<no playlists>" snd (listSelectedElement playlists)
    )
  let playlistSongs = list PlaylistSongs playlistSongsVec 1
  pure HState { chan
              , view
              , mode
              , search
              , searchHistory
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

hmmStartEvent :: HState -> EventM Name HState
hmmStartEvent s = do
  pure s

hBoxPad :: Padding -> [Widget n] -> Widget n
hBoxPad _ []       = emptyWidget
hBoxPad _ [w     ] = w
hBoxPad p (w : ws) = padRight p w <+> hBoxPad p ws

toggleSongMode :: HState -> HState
toggleSongMode s =
  s
    &  modeL
    %~ (\case
         NormalMode   -> SongModeMode
         SongModeMode -> NormalMode
         m            -> m
       )

seekCurEventM :: MPD.FractionalSeconds -> HState -> EventM Name (Next HState)
seekCurEventM i s = do
  _      <- liftIO (withMPD $ MPD.seekCur False i)
  status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
  song   <- liftIO (withMPD MPD.currentSong)
  continue s { currentSong = fromRight Nothing song, status }

handleEvent :: HState -> BrickEvent Name HmmEvent -> EventM Name (Next HState)
handleEvent s e = case e of
  VtyEvent vtye -> case s ^. modeL of
    SearchMode   -> handleSearchEvent s e
    SongModeMode -> case vtye of
      EvKey (KChar 'm') [] -> continue $ toggleSongMode s
      _                    -> continue s
    NormalMode -> case vtye of
      EvKey (KChar 'q') [] -> halt s
      EvKey (KChar 't') [] -> do
        st <- liftIO ((MPD.stState <$>) <$> withMPD MPD.status)
        _  <- case st of
          Left  _           -> liftIO (withMPD $ MPD.pause True)
          Right MPD.Paused  -> liftIO (withMPD $ MPD.play Nothing)
          Right MPD.Stopped -> liftIO (withMPD $ MPD.play Nothing)
          Right MPD.Playing -> liftIO (withMPD $ MPD.pause True)
        continue s
      EvKey (KChar 'm') [] -> continue $ toggleSongMode s
      EvKey (KChar 's') [] -> do
        _ <- liftIO
          (withMPD $ MPD.single (maybe False (not . MPD.stSingle) (status s)))
        status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
        continue s { status }
      EvKey (KChar 'c') [] -> do
        _ <- liftIO
          (withMPD $ MPD.consume (maybe False (not . MPD.stConsume) (status s)))
        status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
        continue s { status }
      EvKey (KChar 'x') [] -> do
        _ <- liftIO
          (withMPD $ MPD.crossfade
            ( (\case
                0 -> 5
                _ -> 0
              )
            $ maybe 0 MPD.stXFadeWidth (status s)
            )
          )
        status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
        continue s { status }
      EvKey (KChar 'r') [] -> do
        _ <- liftIO
          (withMPD $ MPD.repeat (maybe False (not . MPD.stRepeat) (status s)))
        status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
        continue s { status }
      EvKey (KChar 'z') [] -> do
        _ <- liftIO
          (withMPD $ MPD.random (maybe False (not . MPD.stRandom) (status s)))
        status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
        continue s { status }
      EvKey (KChar '/') [] ->
        continue $ s & modeL .~ SearchMode & focusL . focSearchL .~ True
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
    _ -> continue s
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
TODO impliment search history
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
