module UI where


import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.List
import qualified Brick.Util                    as BU
-- import           Brick.Widgets.Border.Style
import           Graphics.Vty.Input.Events
import           Graphics.Vty                   ( defAttr )
import qualified Graphics.Vty                  as Vty
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Song
import           Types
import           Data.Vector                   as V
import           Queue

launch :: IO ()
launch = do
  initialState <- buildInitialState
  _            <- defaultMain app initialState
  pass

app :: App HState e Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const $ attrMap
                        defAttr
                        [ (listSelectedAttr, Vty.withStyle defAttr Vty.reverseVideo)
                        , (listHighlightedAttr, BU.fg Vty.yellow)
                        , (attrName "header", Vty.withStyle defAttr Vty.underline)
                        , (queueAlbumAttr     , BU.fg Vty.red)
                        , (queueTrackAttr     , BU.fg Vty.magenta)
                        , (queueTitleAttr     , BU.fg Vty.cyan)
                        , (queueArtistAttr    , BU.fg Vty.green)
                        , (queueTimeAttr      , BU.fg Vty.blue)
                        ]
  }

queueAttr, queueAlbumAttr, queueTitleAttr, queueTrackAttr, queueArtistAttr, queueTimeAttr
  :: AttrName
queueAttr = "queue"
queueAlbumAttr = queueAttr <> "album"
queueTitleAttr = queueAttr <> "title"
queueTrackAttr = queueAttr <> "track"
queueArtistAttr = queueAttr <> "artist"
queueTimeAttr = queueAttr <> "time"

buildInitialState :: IO HState
buildInitialState = do
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status      <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  playlist    <- fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue = (, False) <$> list Queue0 (V.fromList playlist) 1
  let queueExtent = Nothing
  let clipboard   = list Clipboard V.empty 1
  pure HState { status, currentSong, playlist, queue, queueExtent, clipboard }

drawSong :: HState -> Widget Name
drawSong st = vLimit 3 . center . borderWithLabel (str "Now Playing") $ maybe
  (txt "nothing.")
  queueRow
  ((, False) <$> currentSong st)

drawPlaylist :: HState -> Widget Name
drawPlaylist st =
  let vsize = case queueExtent st of
        Just e  -> (snd . extentSize $ e) - 2
        Nothing -> 40
  in  reportExtent Queue
        .   borderWithLabel (str "Queue")
        .   viewport Queue Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   header
        <=> renderList (const queueRow) True (queue st)

 where
  songIdx = column (Just 4) Max (Pad 1) $ txt "Inx"
  songId  = column (Just 3) Max (Pad 1) $ txt "ID"
  album   = withAttr queueAlbumAttr $ column (Just 25) (Pad 1) Max $ txt "Album"
  track   = withAttr queueTrackAttr $ column (Just 8) Max (Pad 2) $ txt "#"
  title   = withAttr queueTitleAttr $ column Nothing Max Max $ txt "Title"
  artist =
    withAttr queueArtistAttr $ column (Just 25) Max (Pad 1) $ txt "Artist"
  time   = withAttr queueTimeAttr $ column (Just 8) Max (Pad 1) $ txt "Time"
  header = withAttr
    "header"
    (songIdx <+> songId <+> album <+> track <+> title <+> artist <+> time)

queueRow :: (MPD.Song, Highlight) -> Widget n
queueRow (song, hl) =
  (if hl
      then updateAttrMap
        (mapAttrNames
          (   (listHighlightedAttr, )
          <$> [ queueAlbumAttr
              , queueTrackAttr
              , queueTitleAttr
              , queueArtistAttr
              , queueTimeAttr
              ]
          )
        )
      else id
    )
    (   hCenter
    $   songIdx
    <+> songId
    <+> album
    <+> track
    <+> title
    <+> artist
    <+> time
    )
 where
  songIdx =
    column (Just 4) Max (Pad 1) $ txt $ maybe "?" show $ MPD.sgIndex song
  songId =
    column (Just 3) Max (Pad 1)
      $ txt
      $ maybe "?" (\(MPD.Id x) -> show x)
      $ MPD.sgId song
  album = withAttr queueAlbumAttr $ column (Just 25) (Pad 1) Max $ txt $ meta
    "<no album>"
    MPD.Album
    song
  track = withAttr queueTrackAttr $ column (Just 8) Max (Pad 2) $ txt $ meta
    "?"
    MPD.Track
    song
  title = withAttr queueTitleAttr $ column Nothing Max Max $ txt $ meta
    "<no title>"
    MPD.Title
    song
  artist = withAttr queueArtistAttr $ column (Just 25) Max (Pad 1) $ txt $ meta
    "<no artist>"
    MPD.Artist
    song
  time =
    withAttr queueTimeAttr
      $ column (Just 8) Max (Pad 1)
      $ txt
      $ secondsToTime
      $ MPD.sgLength song

column :: Maybe Int -> Padding -> Padding -> Widget n -> Widget n
column maxWidth left right w = case maxWidth of
  Nothing -> wpad
  Just m  -> hLimit m wpad
  where wpad = padLeft left . padRight right $ w

drawUI :: HState -> [Widget Name]
drawUI st = [drawPlaylist st <=> drawSong st]

hBoxPad :: Padding -> [Widget n] -> Widget n
hBoxPad _ []       = emptyWidget
hBoxPad _ [w     ] = w
hBoxPad p (w : ws) = padRight p w <+> hBoxPad p ws


handleEvent :: HState -> BrickEvent Name e -> EventM Name (Next HState)
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
      continue s { queue = listToggleHighlight2 (queue s) }
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
  _ -> continue s


--handleEvent s (VtyEvent e) = continue =<< handleListEventVi handleListEvent e s

{-
TODO write generic Response handler to pring the MPDError instead of doing the thing.
TODO read over the snake guide, implement tick event to read playlist etc.
TODO format playlist better (colors)
TODO impliment borderWithFullLabel
TODO Impliment MPD event channel
TODO impliment song skipping
TODO random song order
-}
