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
import           Hum.Types
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Data.Time                      ( getCurrentTime )
import           Hum.Attributes
import           Hum.Views
import           Hum.Modes
import           Hum.Utils
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

app :: App HState HumEvent Name

app = App { appDraw         = drawUI
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = humStartEvent
          , appAttrMap      = const humAttrMap
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
    <=> if st ^. focusL . focExL
          then txt (st ^. exL . exPrefixL & exPrefixTxt) <+> renderEditor
            (txt . T.unlines)
            (st ^. focusL . focExL)
            (st ^. exL . exEditorL)
          else txt " "
  ]

chooseCursor :: HState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor st ls = if st ^. focusL . focExL
  then find isCurrent ls
  else Nothing
  where isCurrent cl = cl ^. cursorLocationNameL == Just ExEditor

buildInitialState :: BC.BChan HumEvent -> IO HState
buildInitialState chan = do
  let mode = NormalMode
  let ex = ExState { exPrefix        = Cmd
                   , exEditor        = editorText ExEditor (Just 1) ""
                   , searchDirection = True
                   , searchHistory   = []
                   , cmdHistory      = []
                   }
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  queueVec <- V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue = (, False) <$> list QueueList queueVec 1
  artistsVec <- V.fromList . fromRight [] <$> withMPD
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
                    , focEx    = False
                    }
  plListVec <- V.fromList . fromRight [] <$> withMPD MPD.listPlaylists
  let plList = list PlaylistList plListVec 1
  plSongsVec <- V.fromList . fromRight [] <$> withMPD
    ( MPD.listPlaylistInfo
    $ maybe "<no playlists>" snd (listSelectedElement plList)
    )
  let plSongs   = list PlaylistSongs plSongsVec 1
  let library   = LibraryState { artists, albums, songs }
  let playlists = PlaylistsState { plList, plSongs }
  pure HState { chan
              , view
              , mode
              , ex
              , status
              , currentSong
              , queue
              , extentMap
              , clipboard
              , library
              , playlists
              , focus
              }

humStartEvent :: HState -> EventM Name HState
humStartEvent s = pure s

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

handleEvent :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleEvent s e = case e of
  VtyEvent vtye -> case s ^. modeL of
    ExMode   -> handleExEvent s e
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
        _ <-
          liftIO
            ( withMPD
            $ MPD.consume (maybe False (not . MPD.stConsume) (status s))
            )
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
        continue $ s &  modeL .~ ExMode
                     &  exL . exPrefixL .~ FSearch
                     &  exL . searchDirectionL .~ True
                     &  focusL .  focExL .~ True
      EvKey (KChar '?') [] ->
        continue $ s &  modeL .~ ExMode
                     &  exL . exPrefixL .~ BSearch
                     &  exL . searchDirectionL .~ False
                     &  focusL .  focExL .~ True
      EvKey (KChar ':') [] ->
        continue $ s &  modeL .~ ExMode
                     &  exL . exPrefixL .~ Cmd
                     &  focusL .  focExL .~ True
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
    status    <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    continue s { status, extentMap }
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
