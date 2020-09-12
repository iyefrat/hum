{-#LANGUAGE RankNTypes#-}
-- |

module Ham.Views.Playlists where

import           Ham.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Lens.Micro                     ( (^.)
                                                , (%~)
                                                , (.~){- (?~)
                                                  (^.)
                                                , (^?)
                                                , (&)
                                                , (%~)
                                                , _2
                                                , _head
                                                , set
                                                -}
                                                )

import           Brick.Widgets.List
import           Ham.Song
import           Ham.Attributes
import           Ham.Views.Common
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Ham.Utils



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
                          (MPD.toText <$> playlists st)
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
        $   hBorder
        <=> hCenter
              (renderList (const $ playlistSongRow st)
                          ((focPlay . focus $ st) == FocPSongs)
                          (playlistSongs st)
              )
        )

playlistRow :: HState -> T.Text -> Widget n
playlistRow _ val =
  withAttr queueAlbumAttr $ column Nothing (Pad 1) Max $ txt val

playlistSongRow :: HState -> MPD.Song -> Widget n
playlistSongRow st song =
  let pathsInQueue =
          (MPD.sgFilePath <$>) . (fst <$>) . listElements . queue $ st
  in  withAttr
          (if MPD.sgFilePath song `elem` pathsInQueue
            then queueTitleBoldAttr
            else queueTitleAttr
          )
        $ column Nothing (Pad 1) Max
        $ txt (meta (MPD.toText . MPD.sgFilePath $ song) MPD.Title song)

drawViewPlaylists :: HState -> Widget Name
drawViewPlaylists st = drawPlaylistLeft st <+> drawPlaylistRight st

playlistsMove
  :: (forall e . List Name e -> List Name e)
  -> HState
  -> EventM Name (Next HState)
playlistsMove moveFunc s =
  let playfoc = s ^. focusL . focPlayL
  in  case playfoc of
        FocPlaylists -> do
          let playlists' = moveFunc $ playlists s
          playlistSongsVec <- liftIO
            (V.fromList . fromRight [] <$> withMPD
              (MPD.listPlaylistInfo
                (   fromMaybe "<no playlists>"
                $   snd
                <$> listSelectedElement playlists'
                )
              )
            )
          let playlistSongs = list PlaylistSongs playlistSongsVec 1
          continue s { playlists = playlists', playlistSongs }
        FocPSongs -> do
          let playlistSongs' = moveFunc $ playlistSongs s
          continue s { playlistSongs = playlistSongs' }

playlistsAdd :: Bool -> HState -> EventM Name (Next HState)
playlistsAdd play s =
  let playfoc = s ^. focusL . focPlayL
  in  case playfoc of
        FocPlaylists -> do
          songBulkAdd play (listElements (playlistSongs s)) s
        FocPSongs -> do
          let maybeFilePath =
                MPD.sgFilePath . snd <$> listSelectedElement (playlistSongs s)
          traverse_
            (\sel -> liftIO
              (withMPD $ MPD.addId sel Nothing >>= if play
                then MPD.playId
                else const pass
              )
            )
            maybeFilePath
          song <- liftIO (withMPD MPD.currentSong)
          continue s { currentSong = fromRight Nothing song, queue = queue s }

handleEventPlaylists
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventPlaylists s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> playlistsMove listMoveDown s
    EvKey (KChar 'k') [] -> playlistsMove listMoveUp s
    EvKey (KChar 'l') [] -> do
      continue $ s & focusL . focPlayL .~ FocPSongs
    EvKey (KChar 'h') [] -> do
      continue $ s & focusL . focPlayL .~ FocPlaylists
    EvKey KEnter      [] -> playlistsAdd True s
    EvKey (KChar ' ') [] -> playlistsAdd False s
    EvKey (KChar 'G') [] -> playlistsMove (listMoveTo (length . queue $ s)) s
    EvKey (KChar 'g') [] -> playlistsMove (listMoveTo 0) s -- TODO change this to  'gg', somehow
    _                    -> continue s
  _ -> continue s
