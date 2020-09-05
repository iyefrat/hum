-- |

module Ham.Views.Queue where
import           Ham.Types
import           Brick.Main
import           Graphics.Vty.Input.Events
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Ham.Song
import           Ham.Attributes
import           Ham.Queue
import           Ham.Utils
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD

drawNowPlaying :: HState -> Widget Name
drawNowPlaying st = lookupExtent vLimit 5 . center $ maybe (txt "nothing.")
                                                           nowPlaying
                                                           (currentSong st)
 where
  nowPlaying song =
    (txt "\n")
      <=> (hCenter title)
      <=> hCenter (artist <+> txt " - " <+> album)
      <=> progbar
   where
    title     = withAttr queueTitleAttr $ txt $ meta "<no title>" MPD.Title song
    album     = withAttr queueAlbumAttr $ txt $ meta "<no album>" MPD.Album song
    artist    = withAttr queueArtistAttr $ txt $ meta "<no one>" MPD.Artist song
    msongTime = MPD.stTime =<< (status st)
    msongTimeTxt =
      (\(i, j) -> (secondsToTime (round i)) <> "/" <> (secondsToTime (round j)))
        <$> msongTime
    songTime =
      withAttr queueTimeAttr $ txt $ fromMaybe "-:--/-:--" msongTimeTxt
    progbar = withAttr queueTimeAttr $ drawProgressBar st

drawQueue :: HState -> Widget Name
drawQueue st =
  let vsize = case queueExtent st of
        Just e  -> (snd . extentSize $ e)
        Nothing -> 60
  in  reportExtent Queue
        $ hCenter
        $ (   viewport Queue Vertical
          .   visible
          .   vLimit vsize
          .   center
          $   (hCenter {-. hLimit 130-}
                       $ header)
          <=> hBorder
          <=> (hCenter {-. hLimit 130-}
                       $ renderList (const (queueRow st)) True (queue st))
          )

 where
  songIdx = column (Just (Col 4)) Max (Pad 1) $ txt "Inx"
  songId  = column (Just (Col 3)) Max (Pad 1) $ txt "ID"
  album =
    withAttr queueAlbumAttr $ column (Just (Per 25)) (Pad 1) Max $ txt "Album"
  track = withAttr queueTrackAttr $ column (Just (Col 3)) Max (Pad 1) $ txt "#"
  title = withAttr queueTitleAttr $ column Nothing Max Max $ txt "Title"
  artist =
    withAttr queueArtistAttr $ column (Just (Per 25)) Max (Pad 1) $ txt "Artist"
  time =
    withAttr queueTimeAttr $ column (Just (Col 5)) Max (Pad 1) $ txt "Time"
  header = withDefAttr headerAttr
                       ({-songIdx <+> songId <+>-}
                        album <+> track <+> title <+> artist <+> time)

queueRow :: HState -> (MPD.Song, Highlight) -> Widget n
queueRow st (song, hl) =
  (if hl then highlightOverQueueAttrs else id)
    . (if maybe False (MPD.sgIndex song ==) (MPD.sgIndex <$> nowPlaying)
        then withDefAttr queueNowPlayingAttr
        else id
      )
    $ (   hCenter
      $   {-songIdx
    <+> songId
    <+> -}
          album
      <+> track
      <+> title
      <+> artist
      <+> time
      )
 where
  nowPlaying = currentSong st
  songIdx =
    column (Just (Col 4)) Max (Pad 1) $ txt $ maybe "?" show $ MPD.sgIndex song
  songId =
    column (Just (Col 3)) Max (Pad 1)
      $ txt
      $ maybe "?" (\(MPD.Id x) -> show x)
      $ MPD.sgId song
  album =
    withAttr queueAlbumAttr $ column (Just (Per 25)) (Pad 1) Max $ txt $ meta
      "<no album>"
      MPD.Album
      song
  track =
    withAttr queueTrackAttr $ column (Just (Col 3)) Max (Pad 1) $ txt $ meta
      "?"
      MPD.Track
      song
  title = withAttr queueTitleAttr $ column Nothing Max Max $ txt $ meta
    "<no title>"
    MPD.Title
    song
  artist =
    withAttr queueArtistAttr $ column (Just (Per 25)) Max (Pad 1) $ txt $ meta
      "<no artist>"
      MPD.Artist
      song
  time =
    withAttr queueTimeAttr
      $ column (Just (Col 5)) Max (Pad 1)
      $ txt
      $ secondsToTime
      $ MPD.sgLength song
data PerCol = Per Int | Col Int
column :: Maybe (PerCol) -> Padding -> Padding -> Widget n -> Widget n
column maxWidth left right w = case maxWidth of
  Nothing      -> wpad
  Just (Per m) -> hLimitPercent m wpad
  Just (Col m) -> hLimit m wpad
  where wpad = padLeft left . padRight right $ w

drawProgressBar :: HState -> Widget Name
drawProgressBar st = bar
 where
  width    = fromMaybe 0 (fst . extentSize <$> (queueExtent st))
  songTime = fromMaybe (0, 1) (MPD.stTime =<< (status st))
  timeText =
    toString
      . (\(i, j) ->
          (secondsToTime (round i)) <> "/" <> (secondsToTime (round j))
        )
      $ songTime
  completed =
    (\w -> \(i, j) -> round ((i / j) * (fromIntegral w))) width songTime
  bar = str
    (zipWith
      (\a b -> if elem a ("1234567890/:" :: [Char]) then a else b)
      (  (replicate (-5 + div width 2) ' ')
      ++ timeText
      ++ (replicate (-3 + div width 2) ' ')
      )
      (replicate completed '=' ++ replicate (width - completed) ' ')
    )

drawViewQueue :: HState -> [Widget Name]
drawViewQueue st = [drawNowPlaying st <=> drawQueue st]


handleEventQueue
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventQueue s e = case e of
  VtyEvent vtye -> case vtye of
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
      s' <- liftIO rebuildState
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
      s' <- liftIO rebuildState
      continue s'
        { queue     = case mi of
                        Just i  -> listMoveTo i (queue s')
                        Nothing -> queue s'
        , clipboard = c
        }
    _ -> continue s
  _ -> continue s
