module UI where


import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import qualified Brick.BChan                   as BC
import           Brick.Widgets.Core
import           Brick.Widgets.List
import qualified Brick.Util                    as BU
import           Graphics.Vty.Input.Events
import           Graphics.Vty                   ( defAttr )
import qualified Graphics.Vty                  as Vty
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Ham.Types
import qualified Data.Vector                   as V
import           Ham.Queue
import           Data.Time                      ( getCurrentTime )
import           Ham.Attributes
import           Ham.Views.Queue
import           Ham.Views.Library
import           Ham.Views.Common
import           Ham.Utils
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

app :: App HState HamEvent Name

app = App { appDraw         = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = hamStartEvent
          , appAttrMap      = hamAttrMap
          }

drawUI :: HState -> [Widget Name]
drawUI st =
  [ drawNowPlaying st
      <=> (str . show $ ((focLib . focus $ st) == FocSongs))
      <=> (case (view st) of
            QueueView   -> drawViewQueue st
            LibraryView -> drawViewLibrary st
          )
  ]

buildInitialState :: (BC.BChan HamEvent) -> IO HState
buildInitialState chan = do
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  queueVec <- V.fromList <$> fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue = (, False) <$> list QueueList queueVec 1
  currentTime <- getCurrentTime
  artistsVec  <-
    V.fromList <$> fromRight [] <$> (withMPD $ MPD.list MPD.AlbumArtist Nothing)
  let artists = list ArtistsList artistsVec 1
  albumsVec <- albumsOfArtist (snd <$> (listSelectedElement artists))
  let albums    = list AlbumsList albumsVec 1
  let view      = QueueView
  let extentMap = Map.empty
  let clipboard = list Clipboard V.empty 1
  songsVec <- songsOfAlbum (snd <$> (listSelectedElement albums))
  let songs = list SongsList songsVec 1
  let focus = Focus { focQueue = FocQueue, focLib = FocArtists }
  pure HState { chan
              , view
              , status
              , currentSong
              , queue
              , extentMap
              , clipboard
              , currentTime
              , artists
              , songs
              , focus
              }

hamStartEvent :: HState -> EventM Name HState
hamStartEvent s = do
  pure s

hBoxPad :: Padding -> [Widget n] -> Widget n
hBoxPad _ []       = emptyWidget
hBoxPad _ [w     ] = w
hBoxPad p (w : ws) = padRight p w <+> hBoxPad p ws




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
    EvKey (KChar '.') [] -> do
      _    <- liftIO (withMPD MPD.next)
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar ',') [] -> do
      _    <- liftIO (withMPD MPD.previous)
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar ']') [] -> do
      _      <- liftIO (withMPD $ MPD.seekCur False 5)
      status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
      song   <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song, status }
    EvKey (KChar '[') [] -> do
      _      <- liftIO (withMPD $ MPD.seekCur False (-5))
      status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
      song   <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song, status }
    EvKey (KChar '}') [] -> do
      _      <- liftIO (withMPD $ MPD.seekCur False 30)
      status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
      song   <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song, status }
    EvKey (KChar '{') [] -> do
      _      <- liftIO (withMPD $ MPD.seekCur False (-30))
      status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
      song   <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song, status }
    EvKey (KChar '1') [] -> do
      _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
      continue s { view = QueueView }
    EvKey (KChar '2') [] -> do
      _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
      continue s { view = LibraryView }
    EvResize _ _ -> do
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
      continue s { extentMap }
    _ -> case (view s) of
      QueueView   -> handleEventQueue s e
      LibraryView -> handleEventLibrary s e
  (AppEvent (Left Tick)) -> do
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
    status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    currentTime <- liftIO (getCurrentTime)
    continue s { currentTime, status, extentMap }
  (AppEvent (Right (Right _))) -> do
    currentSong <- liftIO (fromRight Nothing <$> withMPD MPD.currentSong)
    status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    queueVec <- liftIO
      (V.fromList <$> fromRight [] <$> withMPD (MPD.playlistInfo Nothing))
    let queueUnmoved = (, False) <$> list QueueList queueVec 1
    let queueNew = case (listSelected (queue s)) of
          Nothing -> queueUnmoved
          Just i  -> listMoveTo i queueUnmoved
    continue s { currentSong, status, queue = queueNew }
  _ -> continue s

--handleEvent s (VtyEvent e) = continue =<< handleListEventVi handleListEvent e s

{-
TODO write generic Response handler to pring the MPDError instead of doing the thing.
-----
TODO AlbumArtist -> Album -> Song
TODO Highlight and add
TODO go over HState, nest? lenses?
TODO Stop and lint everything
TODO go over entire project and tidy up
TODO Various Artists
TODO hackage?
TODO playlists
TODO search!
TODO more vim motions
TODO random song order (and friends)
-}
