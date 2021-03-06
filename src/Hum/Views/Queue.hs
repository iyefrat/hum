
-- | Module    : Hum.Views.Queue
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- Functions for the Queue view.


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

-- | Draw the queue.
drawViewQueue :: HumState -> Widget Name
drawViewQueue st = reportExtent Queue $
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ hCenter
        (   vLimit vsize
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
    withAttr albumAttr $ column (Just (Per 25)) (Pad 1) Max $ txt "Album"
  track = withAttr trackAttr $ column (Just (Col 3)) Max (Pad 1) $ txt "#"
  title = withAttr titleAttr $ column Nothing Max Max $ txt "Title"
  artist =
    withAttr artistAttr $ column (Just (Per 25)) Max (Pad 1) $ txt "Artist"
  time =
    withAttr timeAttr $ column (Just (Col 5)) Max (Pad 1) $ txt "Time"
  header = withDefAttr headerAttr
                       ({-songIdx <+> songId <+>-}
                        album <+> track <+> title <+> artist <+> time)

-- | Draw individual row in queue.
queueRow :: HumState -> (MPD.Song, Highlight) -> Widget n
queueRow st (song, hl) =
  (if hl then highlightOverAttrs else id)
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
    withAttr albumAttr $ column (Just (Per 25)) (Pad 1) Max $ txt $ meta
      "<no album>"
      MPD.Album
      song
  track =
    withAttr trackAttr $ column (Just (Col 3)) Max (Pad 1) $ txt $ meta
      "?"
      MPD.Track
      song
  title = withAttr titleAttr $ column Nothing Max Max $ txt $ meta
    "<no title>"
    MPD.Title
    song
  artist =
    withAttr artistAttr $ column (Just (Per 25)) Max (Pad 1) $ txt $ meta
      "<no artist>"
      MPD.Artist
      song
  time =
    withAttr timeAttr
      $ column (Just (Col 5)) Max (Pad 1)
      $ txt
      $ secondsToTime
      $ MPD.sgLength song

-- | Search queue for next instance of last search.
queueSearch
  :: Bool -- ^ Search direction, True for forward.
  -> HumState
  -> EventM Name HumState
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

-- | Add highlighted songs to given playlist.
queueAddToPl
  :: HumState
  -> String -- ^ Playlist name
  -> EventM Name HumState
queueAddToPl s plName =
  let songs =
        (s ^.  queueL)
          &   getHighlighted
          &   listElements
          <&> fst
  in  songBulkAddtoPl plName songs s

-- | handle key inputs for Queue view.
handleEventQueue
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleEventQueue s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> continue $ s & queueL %~ listMoveDown
    EvKey (KChar 'k') [] -> continue $ s & queueL %~ listMoveUp
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
    EvKey (KChar 'G') [] -> continue $ s & queueL %~ listMoveTo (-1)
    EvKey (KChar 'g') [] -> continue $ s & queueL %~ listMoveTo 0  -- TODO change this to  'gg', somehow
    _ -> continue s
  _ -> continue s
