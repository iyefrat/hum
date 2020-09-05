module UI where


import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
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
import           Ham.Utils

app :: App HState HamEvent Name

app = App { appDraw         = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = hamStartEvent
          , appAttrMap      = hamAttrMap
          }

drawUI :: HState -> [Widget Name]
drawUI st = case (view st) of
  QueueView   -> drawViewQueue st
  LibraryView -> drawViewLibrary st

buildInitialState :: IO HState
buildInitialState = do
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status      <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  queueVec <- V.fromList <$> fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  currentTime <- getCurrentTime
  artistsVec  <-
    V.fromList
    <$> fromRight []
    <$> (withMPD $ MPD.list MPD.AlbumArtistSort Nothing)
  let view        = QueueView
  let queue = (, False) <$> list QueueList queueVec 1
  let queueExtent = Nothing
  let clipboard   = list Clipboard V.empty 1
  let artists     = list ArtistsList artistsVec 1
  pure HState { view
              , status
              , currentSong
              , queueVec
              , queue
              , queueExtent
              , clipboard
              , currentTime
              , artistsVec
              , artists
              }

hamStartEvent :: HState -> EventM Name HState
hamStartEvent s = do
  queueExtent <- lookupExtent Queue
  _           <- liftIO (putStrLn (maybe "nothing" show queueExtent))
  pure (s { queueExtent })

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
      let songTime = fromMaybe 0 $ fst <$> (MPD.stTime =<< (status s))
      _    <- liftIO (withMPD $ MPD.seekCur (songTime + 5))
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar '[') [] -> do
      let songTime = fromMaybe 0 $ fst <$> (MPD.stTime =<< (status s))
      _    <- liftIO (withMPD $ MPD.seekCur (songTime - 5))
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar '}') [] -> do
      let songTime = fromMaybe 0 $ fst <$> (MPD.stTime =<< (status s))
      _    <- liftIO (withMPD $ MPD.seekCur (songTime + 30))
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar '{') [] -> do
      let songTime = fromMaybe 0 $ fst <$> (MPD.stTime =<< (status s))
      _    <- liftIO (withMPD $ MPD.seekCur (songTime - 30))
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey    (KChar '1') [] -> continue s { view = QueueView }
    EvKey    (KChar '2') [] -> continue s { view = LibraryView }
    EvResize _           _  -> do
      queueExtent <- lookupExtent Queue
      continue s { queueExtent }
    _ -> case (view s) of
      QueueView   -> handleEventQueue s e
      LibraryView -> handleEventLibrary s e
  (AppEvent (Left Tick)) -> do
    queueExtent <- lookupExtent Queue
    status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    continue s { status, queueExtent }
  (AppEvent (Right (Right _))) -> do
    s' <- liftIO (rebuildState)
    continue s'
  _ -> continue s

--handleEvent s (VtyEvent e) = continue =<< handleListEventVi handleListEvent e s

{-
TODO write generic Response handler to pring the MPDError instead of doing the thing.
TODO read over the snake guide, implement tick event to read playlist etc.
TODO impliment borderWithFullLabel
TODO impliment song skipping
TODO random song order
TODO more vim motions
TODO understand what is going on with the headerAttr
TODO factor out the collum function somewhat, ugh repition
TODO AlbumArtist -> Album -> Song
TODO search!
TODO find way for seekCur to have +- option
-}
