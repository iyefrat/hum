-- |

module Ham.Views.Common where
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
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

drawNowPlaying :: HState -> Widget Name
drawNowPlaying st = reportExtent NowPlaying $ vLimit 5 . center $ maybe
  (txt "nothing.")
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

drawProgressBar :: HState -> Widget Name
drawProgressBar st = case width of
  0 -> txt $ Prelude.toText timeText
  _ -> bar
 where
  width = fromMaybe
    0
    (fst . extentSize <$> (join $ Map.lookup NowPlaying $ extentMap st))
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

data PerCol = Per Int | Col Int
column :: Maybe PerCol -> Padding -> Padding -> Widget n -> Widget n
column maxWidth left right w = case maxWidth of
  Nothing      -> wpad
  Just (Per m) -> hLimitPercent m wpad
  Just (Col m) -> hLimit m wpad
  where wpad = padLeft left . padRight right $ w
