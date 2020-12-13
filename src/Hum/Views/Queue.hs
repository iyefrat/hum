-- |

module Hum.Views.Queue where
import           Hum.Types
import           Brick.Main
import           Graphics.Vty.Input.Events
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Hum.Attributes
import           Hum.Utils
import           Hum.Rebuild
import           Hum.Views.Common
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Control.Lens

drawViewQueue :: HState -> Widget Name
drawViewQueue st =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ hCenter
        (   viewport Queue Vertical
        .   vLimit vsize
        .   center
        $   hCenter header
        <=> hCenter
              (renderList (const (queueRow st))
                          ((focQueue . focus $ st) == FocQueue)
                          (queue st)
              )
        )

 where
  {-songIdx = column (Just (Col 4)) Max (Pad 1) $ txt "Inx"
  songId  = column (Just (Col 3)) Max (Pad 1) $ txt "ID"-}
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
    . (if Just (MPD.sgIndex song) == (MPD.sgIndex <$> nowPlaying)
        then withDefAttr queueNowPlayingAttr
        else id
      )
    $ hCenter (   {-songIdx
    <+> songId
    <+> -}
               album <+> track <+> title <+> artist <+> time)
 where
  nowPlaying = currentSong st
  {-songIdx =
    column (Just (Col 4)) Max (Pad 1) $ txt $ maybe "?" show $ MPD.sgIndex song
  songId =
    column (Just (Col 3)) Max (Pad 1)
      $ txt
      $ maybe "?" (\(MPD.Id x) -> show x)
      $ MPD.sgId song-}
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


queueSearch :: Bool -> HState -> EventM Name HState
queueSearch direction s =
  let
    dir       = if direction then id else listReverse
    searchkey = fromMaybe "" ((s ^. exL . searchHistoryL) !!? 0)
  in
    if searchkey == ""
      then pure s
      else do
        pure
          $  s
          &  queueL
          %~ ( dir
             . listFindBy
                 ( songSearch searchkey [MPD.Title, MPD.Album, MPD.Artist]
                 . fst
                 )
             . dir
             )

queueAddToPl :: HState -> String -> EventM Name HState
queueAddToPl s plName =
  let songs =
        (s ^.  queueL)
          &   getHighlighted
          &   listElements
          <&> fst
  in  songBulkAddtoPl plName songs s



handleEventQueue
  :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleEventQueue s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> do
      continue $ s & queueL %~ listMoveDown
    EvKey (KChar 'k') [] -> do
      continue $ s & queueL %~ listMoveUp
    EvKey (KChar 'n') [] -> continue =<< queueSearch (s ^. exL . searchDirectionL) s
    EvKey (KChar 'N') [] -> continue =<< queueSearch (s ^. exL . searchDirectionL & not) s
    EvKey (KChar 'a') [] ->
      continue $ s & modeL .~ PromptMode
                   & promptsL . plSelectPromptL .~ listInsert 0 Nothing (Just <$> (s ^. playlistsL . plListL))
                   & promptsL . currentPromptL .~ PlSelectPrompt
                   & promptsL . promptTitleL .~ "Add selected Item(s) to:"
    EvKey KEnter      [] -> do
      let maybeSelectedId =
            MPD.sgId . fst . snd =<< listSelectedElement (queue s)
      traverse_ (\sel -> liftIO (withMPD $ MPD.playId sel)) maybeSelectedId
      rebuildStatus s >>= continue
    EvKey (KChar ' ') [] -> continue $ s & queueL %~ (listMoveDown . listToggleHighlight)
    EvKey (KChar 'd') [] -> do
      let clSongs' = s ^. queueL & getHighlighted
      _ <- liftIO (withMPD $ deleteHighlightedfromQ (s ^. queueL))
      let s' = s & clipboardL . clSongsL .~ clSongs'
      rebuildQueue s' >>= rebuildStatus >>= continue
    EvKey (KChar 'D') [] -> do
      let clSongs' = s^. queueL
      let s' = s & clipboardL . clSongsL .~ clSongs'
      _ <- liftIO (withMPD MPD.clear)
      rebuildQueue s' >>= rebuildStatus >>= continue
    EvKey (KChar 'y') [] -> continue $ yankHighlighted s queueL
    EvKey (KChar 'p') [] -> do
      let clSongs' = s ^. clipboardL . clSongsL
      _ <- liftIO (withMPD $ pasteSongstoQ clSongs' (s ^. queueL))
      rebuildQueue s >>= rebuildStatus >>= continue
    EvKey (KChar 'G') [] -> do
      continue $ s & queueL %~ listMoveTo (-1)
    EvKey (KChar 'g') [] -> do -- TODO change this to  'gg', somehow
      continue $ s & queueL %~ listMoveTo 0
    _ -> continue s
  _ -> continue s
