module UI where


import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Brick.Util
-- import           Brick.Widgets.Border.Style
import           Graphics.Vty.Input.Events
import           Graphics.Vty                   ( defAttr )
import qualified Graphics.Vty                  as Vty
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Song
import           Data.Vector                   as V

launch :: IO ()
launch = do
  initialState <- buildInitialState
  endState     <- defaultMain app initialState
  print endState

data HState =
  HState { status :: Maybe MPD.Status,
           currentSong :: Maybe MPD.Song,
           playlist :: [ MPD.Song ] ,
           queue :: List Name MPD.Song,
           queueExtent :: Maybe (Extent Name)}
  deriving (Show) --, Eq)

-- data Name =
--  Name
--  deriving (Show, Eq, Ord)
data Name = Queue | Queue0
 deriving (Show, Eq, Ord)

app :: App HState e Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const $ attrMap
                        defAttr
                        [ (listSelectedAttr, Vty.black `Brick.Util.on` Vty.white)
                        , (attrName "header", Vty.withStyle defAttr Vty.underline)
                        ]
  }

buildInitialState :: IO HState
buildInitialState = do
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status      <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  playlist    <- fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue       = list Queue0 (V.fromList playlist) 1
  let queueExtent = Nothing
  pure HState { status, currentSong, playlist, queue, queueExtent }

drawSong :: HState -> Widget Name
drawSong st =
  vLimit 3 . center . borderWithLabel (str "Now Playing") $ fromMaybe
    (txt "nothing.")
    (queueRow <$> (currentSong st))

drawPlaylist :: HState -> Widget Name
drawPlaylist st =
  let vsize = case queueExtent st of
        Just e  -> (snd . extentSize $ e) - 2
        Nothing -> 40
  in  reportExtent Queue
        $   borderWithLabel (str "Queue (under construction)")
        $   viewport Queue Vertical
        $   visible
        $   vLimit vsize
        .   center
        $   header
        <=> (renderList (const queueRow) True (queue st))

 where
  songIdx  = column (Just 4) Max (Pad 1) $ txt "Inx"
  songId   = column (Just 3) Max (Pad 1) $ txt "ID"
  album    = column (Just 25) (Pad 1) Max $ txt "Album"
  track    = column (Just 8) Max (Pad 2) $ txt "#"
  title    = column Nothing Max Max $ txt "Title"
  artist   = column (Just 25) Max (Pad 1) $ txt "Artist"
  duration = column (Just 8) Max (Pad 1) $ txt "Time"
  header   = withAttr
    ("header")
    (songIdx <+> songId <+> album <+> track <+> title <+> artist <+> duration)

queueRow :: MPD.Song -> Widget n
queueRow song =
  hCenter
    $   songIdx
    <+> songId
    <+> album
    <+> track
    <+> title
    <+> artist
    <+> duration
 where
  songIdx =
    column (Just 4) Max (Pad 1) $ txt $ maybe "?" show $ MPD.sgIndex song
  songId =
    column (Just 3) Max (Pad 1)
      $ txt
      $ maybe "?" (\(MPD.Id x) -> show x)
      $ MPD.sgId song
  album = column (Just 25) (Pad 1) Max $ txt $ meta "<no album>" MPD.Album song
  track = column (Just 8) Max (Pad 2) $ txt $ meta "?" MPD.Track song
  title = column Nothing Max Max $ txt $ meta "<no title>" MPD.Title song
  artist =
    column (Just 25) Max (Pad 1) $ txt $ meta "<no artist>" MPD.Artist song
  duration =
    column (Just 8) Max (Pad 1) $ txt $ secondsToTime $ MPD.sgLength song

column :: Maybe Int -> Padding -> Padding -> Widget n -> Widget n
column maxWidth left right w = case maxWidth of
  Nothing -> wpad
  Just m  -> hLimit m wpad
  where wpad = padLeft left . padRight right $ w

drawUI :: HState -> [Widget Name]
drawUI st = [(<=>) (drawPlaylist st) (drawSong st)]

hBoxPad :: Padding -> [Widget n] -> Widget n
hBoxPad _ []       = emptyWidget
hBoxPad _ [w     ] = w
hBoxPad p (w : ws) = padRight p w <+> hBoxPad p ws


handleEvent :: HState -> BrickEvent Name e -> EventM Name (Next HState)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey (KChar 'p') [] -> do
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
    EvKey (KChar 'j') [] -> do
      queueExtent <- lookupExtent Queue
      continue s { queue = listMoveDown $ queue s, queueExtent }
    EvKey (KChar 'k') [] -> do
      queueExtent <- lookupExtent Queue
      continue s { queue = listMoveUp $ queue s, queueExtent }
    EvKey KEnter [] -> do
      let maybeSelectedId =
            join ((MPD.sgId . snd) <$> (listSelectedElement (queue s)))
      traverse_ (\sel -> liftIO (withMPD $ MPD.playId sel)) maybeSelectedId
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song, queue = queue s }

    EvResize _ _ -> do
      queueExtent <- lookupExtent Queue
      continue s { queueExtent }
    _ -> continue s
  _ -> continue s

--handleEvent s (VtyEvent e) = continue =<< handleListEventVi handleListEvent e s

{-
TODO write generic Response handler to pring the MPDError instead of doing the thing.
TODO read over the snake guide, implement tick event to read playlist etc.
TODO format playlist better (colors)
TODO impliment borderWithFullLabel
TODO Impliment MPD event channel
TODO impliment song skipping
TODO impliment song deletion, random?
-}
