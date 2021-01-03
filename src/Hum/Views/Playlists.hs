
-- | Module    : Hum.Views.Library
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- Functions for the Playlist view.

module Hum.Views.Playlists where

import           Hum.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Control.Lens
import           Brick.Widgets.List
import           Hum.Attributes
import           Hum.Views.Common
import           Hum.Rebuild
import qualified Data.Text                     as T
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Hum.Utils

-- | Draw left column in Playlist view.
drawPlaylistLeft :: HumState -> Widget Name
drawPlaylistLeft st =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ reportExtent PlaylistLeft $ hCenter
        (   viewport PlaylistLeft Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ playlistRow st)
                          ((focPlay . focus $ st) == FocPlaylists)
                          (MPD.toText <$> st ^. playlistsL . plListL)
              )
        )

-- | Draw right column in Playlist view.
drawPlaylistRight :: HumState -> Widget Name
drawPlaylistRight st =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $  reportExtent PlaylistRight $ hCenter
        (   viewport PlaylistRight Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   (if st ^. editableL
              then
                withDefAttr listHighlightedAttr
                . withBorderStyle (borderStyleFromChar '=')
                $ hBorderWithLabel
                $ txt "editing"
              else hBorder
            )
        <=> hCenter
              (renderList (const $ playlistSongRow st)
                          ((focPlay . focus $ st) == FocPSongs)
                          (st ^. playlistsL . plSongsL)
              )
        )

-- | Draw row in playlist column in Playlist view.
playlistRow :: HumState -> T.Text -> Widget n -- TODO rename?
playlistRow _ val =
  withAttr albumAttr $ column Nothing (Pad 1) Max $ txt val

-- | Draw row in song column in Playlist view.
playlistSongRow :: HumState -> (MPD.Song,Highlight) -> Widget n
playlistSongRow st (song,hl) =
  let pathsInQueue =
        (MPD.sgFilePath <$>) . (fst <$>) . listElements . queue $ st
  in (if hl then highlightOverAttrs else id) . withAttr
          (if MPD.sgFilePath song `elem` pathsInQueue
            then titleBoldAttr
            else titleAttr
          )
        $ column Nothing (Pad 1) Max
        $ txt (meta (MPD.toText . MPD.sgFilePath $ song) MPD.Title song)

-- | Draw Playlist view.
drawViewPlaylists :: HumState -> Widget Name
drawViewPlaylists st =
  hLimitPercent 25 (drawPlaylistLeft st) <+> drawPlaylistRight st

-- | Move focused playlist column by given function
playlistsMove
  :: (forall e . List Name e -> List Name e) -- ^ Function to move the focused column with
  -> HumState
  -> EventM Name HumState
playlistsMove moveFunc s =
  let playfoc = s ^. focusL . focPlayL
  in  case playfoc of
        FocPlaylists -> rebuildPlList $ s & playlistsL . plListL %~ moveFunc
        FocPSongs    -> do
          pure $ s & playlistsL . plSongsL %~ moveFunc

-- | Add selected element in Playlist view to queue.
-- If the element is a playlist adds entire playlist.
playlistsAddtoQ
  :: Bool -- ^ Play first item added to queue
  -> HumState
  -> EventM Name HumState
playlistsAddtoQ play s =
  let playfoc = s ^. focusL . focPlayL
  in  case playfoc of
        FocPlaylists -> do
          songBulkAddtoQ play (fst <$> listElements (s ^. playlistsL . plSongsL)) s
        FocPSongs -> do
          let maybeFilePath = MPD.sgFilePath . fst . snd <$> listSelectedElement
                (s ^. playlistsL . plSongsL)
          traverse_
            (\sel -> liftIO
              (withMPD $ MPD.addId sel Nothing >>= if play
                then MPD.playId
                else const pass
              )
            )
            maybeFilePath
          song <- liftIO (withMPD MPD.currentSong)
          pure s { currentSong = fromRight Nothing song, queue = queue s }

-- | Search focused playlist column for next instance of last search.
playlistsSearch
  :: Bool -- ^ Search direction, True for forward.
  -> HumState
  -> EventM Name HumState
playlistsSearch direction s =
  let playfoc   = s ^. focusL . focPlayL
      dir       = if direction then id else listReverse
      searchkey = fromMaybe "" ((s ^. exL . searchHistoryL) !!? 0)
  in  if searchkey == ""
        then pure s
        else case playfoc of
          FocPlaylists -> do
            rebuildPlList
              $  s
              &  playlistsL
              .  plListL
              %~ (dir . listFindBy (stringySearch searchkey) . dir)
          FocPSongs -> do
            pure
              $  s
              &  playlistsL
              .  plSongsL
              %~ (dir . listFindBy (songSearch searchkey [MPD.Title] . fst) . dir)

-- | handle key inputs for Playlist view.
handleEventPlaylists
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleEventPlaylists s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> continue =<< playlistsMove listMoveDown s
    EvKey (KChar 'k') [] -> continue =<< playlistsMove listMoveUp s
    EvKey (KChar 'l') [] -> continue $ s & focusL . focPlayL .~ FocPSongs
    EvKey (KChar 'h') [] -> continue $ s & focusL . focPlayL .~ FocPlaylists
    EvKey (KChar 'n') [] ->
      continue =<< playlistsSearch (s ^. exL . searchDirectionL) s
    EvKey (KChar 'N') [] ->
      continue =<< playlistsSearch (s ^. exL . searchDirectionL & not) s
    EvKey KEnter [] ->
      continue =<< playlistsMove listMoveDown =<< playlistsAddtoQ True s
    EvKey (KChar ' ') [] -> if s ^. editableL then
      continue $ s & playlistsL . plSongsL %~ (listMoveDown . listToggleHighlight)
      else
      continue =<< playlistsMove listMoveDown =<< playlistsAddtoQ False s
    EvKey (KChar 'd') []
       | s ^. editableL
         -> continue $ deleteHighlighted s (playlistsL . plSongsL)
       | s ^. focusL . focPlayL == FocPlaylists
         -> continue $ s & modeL .~ PromptMode
                         & promptsL . currentPromptL .~ YNPrompt
                         & promptsL . promptTitleL .~ ("DELETE " <> selectedPl <> "?\nYou can't paste it back yet")
                         & promptsL . exitPromptFuncL .~ deleteSelectedPl
       | otherwise ->  continue s
    EvKey (KChar 'y') []
       | s ^. editableL
         -> continue $ s & clipboardL . clSongsL .~ (s ^. playlistsL . plSongsL & getHighlighted)
       | s ^. focusL . focPlayL == FocPlaylists
         -> continue $ s & clipboardL . clPlNameL .~ (s ^. playlistsL . plListL & listSelectedElement <&> snd)
       | otherwise -> continue s
    EvKey (KChar 'p') []
       | s ^. editableL
         -> continue $ s & playlistsL . plSongsL %~ listPaste (s^. clipboardL. clSongsL)
       | s ^. focusL . focPlayL == FocPlaylists
         -> continue =<< pastePlaylist s
       | otherwise -> continue s
    EvKey (KChar 'G') [] -> continue =<< playlistsMove (listMoveTo (-1)) s
    EvKey (KChar 'g') [] -> continue =<< playlistsMove (listMoveTo 0) s -- TODO change this to  'gg', somehow
    EvKey (KChar 'e') [] -> if s ^. editableL then
      continue $ s & editableL %~ not
                   & modeL .~ PromptMode
                   & promptsL . currentPromptL .~ YNPrompt
                   & promptsL . promptTitleL .~ ("Save changes to " <> selectedPl <> "?")
                   & promptsL . exitPromptFuncL .~ saveEditedPl
      else continue =<< rebuildPlList (s & editableL %~ not)
    _                    -> continue s
  _ -> continue s
  where selectedPl = s ^. playlistsL . plListL & listSelectedElement ? maybe "<error>" snd ? MPD.toText
