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
import           Ham.Views.Common
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Vector                   as V

drawViewQueue :: HState -> Widget Name
drawViewQueue st =
  let vsize = case (join $ Map.lookup Queue $ extentMap st) of
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
          <=> (hCenter {-. hLimit 130-}
                       $ renderList (const (queueRow st))
                                    ((focQueue . focus $ st) == FocQueue)
                                    (queue st)
              )
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

handleEventQueue
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventQueue s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> do
      extentMap <- updateExtentMap
      continue s { queue = listMoveDown $ queue s, extentMap }
    EvKey (KChar 'k') [] -> do
      extentMap <- updateExtentMap
      continue s { queue = listMoveUp $ queue s, extentMap }
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
      extentMap   <- updateExtentMap
      currentSong <- liftIO (fromRight Nothing <$> withMPD MPD.currentSong)
      status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
      queueVec    <- liftIO
        (V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing))
      let queue = (, False) <$> list QueueList queueVec 1
      continue s
        { currentSong
        , status
        , queue       = case mi of
                          Just i  -> listMoveTo i queue
                          Nothing -> queue
        , clipboard
        , extentMap
        }
    EvKey (KChar 'y') [] -> do
      continue s { clipboard = getHighlighted (queue s) }
    EvKey (KChar 'p') [] -> do
      let c = clipboard s
      _ <- liftIO (withMPD $ pasteClipboard c (queue s))
      let mi = listSelected (queue s)
      extentMap   <- updateExtentMap
      currentSong <- liftIO (fromRight Nothing <$> withMPD MPD.currentSong)
      status <- liftIO (fromRight Nothing <$> (Just <<$>> withMPD MPD.status))
      queueVec    <- liftIO
        (V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing))
      let queue = (, False) <$> list QueueList queueVec 1
      continue s
        { currentSong
        , status
        , queue       = case mi of
                          Just i  -> listMoveTo i queue
                          Nothing -> queue
        , clipboard   = c
        , extentMap
        }
    _ -> continue s
  _ -> continue s
