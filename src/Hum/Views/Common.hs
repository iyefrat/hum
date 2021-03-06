
-- | Module    : Hum.Views.Library
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- Functions for the Help view.

module Hum.Views.Common where
import           Hum.Types
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Brick.Widgets.Border
import           Hum.Attributes
import           Hum.Utils
import qualified Network.MPD                   as MPD
import qualified Data.Text                     as T
import           Brick.Widgets.List
import           Control.Lens

-- | Draw Now Playing box.
drawNowPlaying :: HumState -> Widget Name
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
    title = withAttr titleAttr $ txt $ meta "<no title>" MPD.Title song
    album =
      withAttr albumAttr (txt $ meta "<no album>" MPD.Album song)
        <+> txt " ("
        <+> withAttr dateAttr (txt $ meta "????" MPD.Date song)
        <+> txt ")"
    artist  = withAttr artistAttr $ txt $ meta "<no one>" MPD.Artist song
    progbar = withAttr timeAttr $ drawProgressBar st
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

-- | Draw progress bar for song time.
drawProgressBar :: HumState -> Widget Name
drawProgressBar st =
  Widget Fixed Fixed  $ do
    ctx <- getContext
    let width = ctx ^. windowWidthL
    render $ bar width
 where
  songTime = fromMaybe (0, 1) (MPD.stTime =<< status st)
  timeText =
    toString
      . (\(i, j) -> secondsToTime (round i) <> "/" <> secondsToTime (round j))
      $ songTime
  completed width = (\w (i, j) -> round ((i / j) * fromIntegral w)) width songTime
  bar width     = str
    (zipWith
      (\a b -> if a `elem` ("1234567890/:" :: String) then a else b)
      (replicate (-5 + div width 2) ' ' ++ timeText ++ replicate
        (-3 + div width 2)
        ' '
      )
      (replicate (completed width) '=' ++ replicate (width - (completed width)) ' ')
    )

-- | Either a number or a percent. To be used for widget horizontal size.
data PerCol =
    Per Int -- ^ percent size
  | Col Int -- ^ column number size

-- | Helper function for drawing column rows.
column
  :: Maybe PerCol -- ^ Maximum width, greedy if Nothing.
  -> Padding -- ^ Left padding
  -> Padding -- ^ Right padding
  -> Widget n
  -> Widget n
column maxWidth left right w = case maxWidth of
  Nothing      -> wpad
  Just (Per m) -> hLimitPercent m wpad
  Just (Col m) -> hLimit m wpad
  where wpad = padLeft left . padRight right $ w

-- | Returns True if text is substring of one of the given tags of the given song.
songSearch :: Text -> [MPD.Metadata] -> MPD.Song -> Bool
songSearch text metadata song =
  let mtags = (T.toLower <$>) . (`mmeta` song) <$> metadata
  in  or $ fromMaybe False <$> (T.isInfixOf (T.toLower text) <<$>> mtags)

-- | Returns True if text is substring of the given strings.
stringySearch :: MPD.ToString a => Text -> a -> Bool
stringySearch text value =
  T.isInfixOf (T.toLower text) (T.toLower . MPD.toText $ value)

-- | Draws a prompt.
drawPrompt :: HumState -> Widget Name
drawPrompt st = case st ^. promptsL . currentPromptL of
  PlSelectPrompt ->
    centerLayer
      .   border
      .   setAvailableSize (30, 10)
      .   center
      $   (hCenter . txt $ st ^. promptsL . promptTitleL)
      <=> hBorder
      <=> renderListWithIndex drawPlSelectRow
                              True
                              (st ^. promptsL . plSelectPromptL)
  TextPrompt ->
    centerLayer
      .   border
      .   setAvailableSize (30, 3)
      .   center
      $   (hCenter . txt $ st ^. promptsL . promptTitleL)
      <=> hBorder
      <=> (center . setAvailableSize (25, 1) . withDefAttr editorAttr)
            (renderEditor (txt . T.unlines) True (st ^. promptsL . textPromptL))
  YNPrompt ->
    centerLayer
      .   border
      .   hLimit 30
      $   (hCenter . txt $ st ^. promptsL . promptTitleL)
      <=> (hCenter . txt $ "[y/n]")

-- | Draw row in playlist select prompt.
drawPlSelectRow :: Int -> Bool -> Maybe MPD.PlaylistName -> Widget n
drawPlSelectRow i _ pl = if i==0 then
  str "New Playlist" <=> modifyDefAttr (const wobAttr) hBorder
  else str (MPD.toString $ fromMaybe "<error getting playlist name>" pl)
