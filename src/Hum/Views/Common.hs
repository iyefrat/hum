{-# LANGUAGE LambdaCase #-}
-- |

module Hum.Views.Common where
import           Hum.Types
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Hum.Attributes
import           Hum.Utils
import           Hum.Rebuild
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V
import qualified Data.Text                     as T

drawNowPlaying :: HState -> Widget Name
drawNowPlaying st = reportExtent NowPlaying $ vLimit 5 . center $ maybe
  (txt "nothing.")
  nowPlaying
  (currentSong st)
 where
  nowPlaying song =
    txt "\n"
      <=> hCenter title
      <=> hCenter (artist <+> txt " - " <+> album)
      <=> progbar
      <=> (padRight Max playing <+> padLeft Max mode)
   where
    title = withAttr queueTitleAttr $ txt $ meta "<no title>" MPD.Title song
    album =
      withAttr queueAlbumAttr (txt $ meta "<no album>" MPD.Album song)
        <+> txt " ("
        <+> withAttr queueDateAttr (txt $ meta "????" MPD.Date song)
        <+> txt ")"
    artist  = withAttr queueArtistAttr $ txt $ meta "<no one>" MPD.Artist song
    progbar = withAttr queueTimeAttr $ drawProgressBar st
    playing = txt $ maybe
      "[       ]"
      ((\t -> "[" <> t <> "]") . T.toLower . show . MPD.stState)
      (status st)
    formatMode t modeFun = maybe
      "-"
      ( (\case
          False -> "-"
          True  -> t
        )
      . modeFun
      )
      (status st)
    repeatmpd = formatMode "r" MPD.stRepeat
    random    = formatMode "z" MPD.stRandom
    single    = formatMode "s" MPD.stSingle
    consume   = formatMode "c" MPD.stConsume
    crossfade = formatMode "x" ((/= 0) . MPD.stXFadeWidth)
    mode =
      txt $ "[" <> repeatmpd <> random <> single <> consume <> crossfade <> "]"

drawProgressBar :: HState -> Widget Name
drawProgressBar st = case width of
  0 -> txt $ Prelude.toText timeText
  _ -> bar
 where
  width =
    maybe 0 (fst . extentSize) (join (Map.lookup NowPlaying $ extentMap st))
  songTime = fromMaybe (0, 1) (MPD.stTime =<< status st)
  timeText =
    toString
      . (\(i, j) -> secondsToTime (round i) <> "/" <> secondsToTime (round j))
      $ songTime
  completed = (\w (i, j) -> round ((i / j) * fromIntegral w)) width songTime
  bar       = str
    (zipWith
      (\a b -> if a `elem` ("1234567890/:" :: [Char]) then a else b)
      (replicate (-5 + div width 2) ' ' ++ timeText ++ replicate
        (-3 + div width 2)
        ' '
      )
      (replicate completed '=' ++ replicate (width - completed) ' ')
    )

--drawEx :: HState -> Widget Name
--drawEx st =

data PerCol = Per Int | Col Int
column :: Maybe PerCol -> Padding -> Padding -> Widget n -> Widget n
column maxWidth left right w = case maxWidth of
  Nothing      -> wpad
  Just (Per m) -> hLimitPercent m wpad
  Just (Col m) -> hLimit m wpad
  where wpad = padLeft left . padRight right $ w

songBulkAddtoQ :: Bool -> V.Vector MPD.Song -> HState -> EventM n HState
songBulkAddtoQ play songs s = do
  let songPaths = MPD.sgFilePath <$> songs
  traverse_
    (\sel -> liftIO
      (withMPD $ MPD.addId sel Nothing >>= if play
        then MPD.playId
        else const pass
      )
    )
    (V.take 1 songPaths)
  traverse_ (\sel -> liftIO (withMPD $ MPD.addId sel Nothing))
            (V.drop 1 songPaths)
  song <- liftIO (withMPD MPD.currentSong)
  pure s { currentSong = fromRight Nothing song, queue = queue s }

songBulkAddtoPl :: String -> V.Vector MPD.Song -> HState -> EventM n HState
songBulkAddtoPl pl songs s = do
  let songPaths = MPD.sgFilePath <$> songs
  traverse_
    (\sel -> liftIO
      (withMPD $ MPD.playlistAdd (fromString pl) sel
      )
    )
    songPaths
  rebuildPl s


songSearch :: Text -> [MPD.Metadata] -> MPD.Song -> Bool
songSearch text metadata song =
  let mtags = (T.toLower <$>) . (\tag -> mmeta tag song) <$> metadata
  in  or $ fromMaybe False <$> (T.isInfixOf (T.toLower text) <<$>> mtags)


stringySearch :: MPD.ToString a => Text -> a -> Bool
stringySearch text value =
  T.isInfixOf (T.toLower text) (T.toLower . MPD.toText $ value)
