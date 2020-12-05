{-#LANGUAGE RankNTypes#-}
-- |

module Hum.Views.Playlists where

import           Hum.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Lens.Micro
import           Brick.Widgets.List
import           Hum.Attributes
import           Hum.Views.Common
import           Hum.Rebuild
import qualified Data.Text                     as T
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Hum.Utils



drawPlaylistLeft :: HState -> Widget Name
drawPlaylistLeft st =
  let vsize = case join $ Map.lookup PlaylistLeft $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent PlaylistLeft $ hCenter
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
drawPlaylistRight :: HState -> Widget Name
drawPlaylistRight st =
  let vsize = case join $ Map.lookup PlaylistRight $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent PlaylistRight $ hCenter
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

playlistRow :: HState -> T.Text -> Widget n
playlistRow _ val =
  withAttr queueAlbumAttr $ column Nothing (Pad 1) Max $ txt val

playlistSongRow :: HState -> (MPD.Song,Highlight) -> Widget n
playlistSongRow st (song,hl) =
  let pathsInQueue =
        (MPD.sgFilePath <$>) . (fst <$>) . listElements . queue $ st
  in (if hl then highlightOverQueueAttrs else id) . withAttr
          (if MPD.sgFilePath song `elem` pathsInQueue
            then queueTitleBoldAttr
            else queueTitleAttr
          )
        $ column Nothing (Pad 1) Max
        $ txt (meta (MPD.toText . MPD.sgFilePath $ song) MPD.Title song)






drawViewPlaylists :: HState -> Widget Name
drawViewPlaylists st =
  hLimitPercent 25 (drawPlaylistLeft st) <+> drawPlaylistRight st

playlistsMove
  :: (forall e . List Name e -> List Name e) -> HState -> EventM Name HState
playlistsMove moveFunc s =
  let playfoc = s ^. focusL . focPlayL
  in  case playfoc of
        FocPlaylists -> rebuildPlList $ s & playlistsL . plListL %~ moveFunc
        FocPSongs    -> do
          pure $ s & playlistsL . plSongsL %~ moveFunc

playlistsAddtoQ :: Bool -> HState -> EventM Name HState
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

playlistsSearch :: Bool -> HState -> EventM Name HState
playlistsSearch direction s =
  let playfoc   = s ^. focusL . focPlayL
      dir       = if direction then id else listReverse
      searchkey = fromMaybe "" ((s ^. exL . searchHistoryL) !!? 0)
  in  if searchkey == ""
        then pure s
        else case playfoc of
          FocPlaylists -> do
            extentMap <- updateExtentMap
            rebuildPlList
              $  (s { extentMap })
              &  playlistsL
              .  plListL
              %~ (dir . listFindBy (stringySearch searchkey) . dir)
          FocPSongs -> do
            extentMap <- updateExtentMap
            pure
              $  s { extentMap }
              &  playlistsL
              .  plSongsL
              %~ (dir . listFindBy (songSearch searchkey [MPD.Title] . fst) . dir)


handleEventPlaylists
  :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
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
    EvKey (KChar 'G') [] ->
      continue =<< playlistsMove (\ls -> listMoveBy (length ls) ls) s
    EvKey (KChar 'g') [] -> continue =<< playlistsMove (listMoveTo 0) s -- TODO change this to  'gg', somehow
    EvKey (KChar 'e') [] -> continue $ s & editableL %~ not
    _                    -> continue s
  _ -> continue s
