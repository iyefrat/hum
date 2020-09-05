-- |

module Ham.Views.Queue where
import           Ham.Types
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Ham.Song
import           Ham.Attributes
import qualified Network.MPD                   as MPD

drawNowPlaying :: HState -> Widget Name
drawNowPlaying st = vLimit 5 . center $ maybe (txt "nothing.")
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
  in
    reportExtent Queue
    $ hCenter
    $ (   viewport Queue Vertical
      .   visible
      .   vLimit vsize
      .   center
      $   (hCenter . hLimit 130 $ header)
      <=> hBorder
      <=> (hCenter . hLimit 130 $ renderList (const queueRow) True (queue st))
      )

 where
  songIdx = column (Just 4) Max (Pad 1) $ txt "Inx"
  songId  = column (Just 3) Max (Pad 1) $ txt "ID"
  album   = withAttr queueAlbumAttr $ column (Just 25) (Pad 1) Max $ txt "Album"
  track   = withAttr queueTrackAttr $ column (Just 3) Max (Pad 1) $ txt "#"
  title   = withAttr queueTitleAttr $ column Nothing Max Max $ txt "Title"
  artist =
    withAttr queueArtistAttr $ column (Just 25) Max (Pad 1) $ txt "Artist"
  time   = withAttr queueTimeAttr $ column (Just 5) Max (Pad 1) $ txt "Time"
  header = withAttr headerAttr
                    ({-songIdx <+> songId <+>-}
                     album <+> track <+> title <+> artist <+> time)

queueRow :: (MPD.Song, Highlight) -> Widget n
queueRow (song, hl) = (if hl then highlightOverQueueAttrs else id)
  (   hCenter
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
  track = withAttr queueTrackAttr $ column (Just 3) Max (Pad 1) $ txt $ meta
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
      $ column (Just 5) Max (Pad 1)
      $ txt
      $ secondsToTime
      $ MPD.sgLength song

column :: Maybe Int -> Padding -> Padding -> Widget n -> Widget n
column maxWidth left right w = case maxWidth of
  Nothing -> wpad
  Just m  -> hLimit m wpad
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
