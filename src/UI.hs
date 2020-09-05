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

app :: App HState HamEvent Name
app = App { appDraw         = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = hamStartEvent
          , appAttrMap      = hamAttrMap
          }

drawUI :: HState -> [Widget Name]
drawUI = drawViewQueue

buildInitialState :: IO HState
buildInitialState = do
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status      <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  playlist    <- fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  currentTime <- getCurrentTime
  let queue = (, False) <$> list QueueList (V.fromList playlist) 1
  let queueExtent = Nothing
  let clipboard   = list Clipboard V.empty 1
  pure HState { status
              , currentSong
              , playlist
              , queue
              , queueExtent
              , clipboard
              , currentTime
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
      _    <- liftIO (withMPD $ MPD.seekId (songTime + 5))
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar '[') [] -> do
      _    <- liftIO (withMPD MPD.previous)
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar 'j') [] -> do
      queueExtent <- lookupExtent Queue
      continue s { queue = listMoveDown $ queue s, queueExtent }
    EvKey (KChar 'k') [] -> do
      queueExtent <- lookupExtent Queue
      continue s { queue = listMoveUp $ queue s, queueExtent }
    EvKey KEnter [] -> do
      let maybeSelectedId =
            MPD.sgId . fst . snd =<< listSelectedElement (queue s)
      traverse_ (\sel -> liftIO (withMPD $ MPD.playId sel)) maybeSelectedId
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song, queue = queue s }
    EvKey (KChar ' ') [] -> do
      continue s { queue = listToggleHighlight (queue s) }
    EvKey (KChar 'd') [] -> do
      let clipboard = getHighlighted (queue s)
      _ <- liftIO (withMPD $ deleteHighlighted (queue s))
      let mi = listSelected (queue s)
      s' <- liftIO buildInitialState
      continue s'
        { queue     = case mi of
                        Just i  -> listMoveTo i (queue s')
                        Nothing -> queue s'
        , clipboard
        }
    EvKey (KChar 'y') [] -> do
      continue s { clipboard = getHighlighted (queue s) }
    EvKey (KChar 'p') [] -> do
      let c = clipboard s
      _ <- liftIO (withMPD $ pasteClipboard c (queue s))
      let mi = listSelected (queue s)
      s' <- liftIO buildInitialState
      continue s'
        { queue     = case mi of
                        Just i  -> listMoveTo i (queue s')
                        Nothing -> queue s'
        , clipboard = c
        }
    EvResize _ _ -> do
      queueExtent <- lookupExtent Queue
      continue s { queueExtent }
    _ -> continue s
  (AppEvent (Left Tick)) -> do
    status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    continue s { status }
  (AppEvent (Right (Right _))) -> do
    currentSong <- liftIO (fromRight Nothing <$> withMPD MPD.currentSong)
    status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
    playlist    <- liftIO (fromRight [] <$> withMPD (MPD.playlistInfo Nothing))
    currentTime <- liftIO (getCurrentTime)
    continue s { currentSong, status, playlist, currentTime }
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
TODO progressbar for song
-}
