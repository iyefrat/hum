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
                                                , (%~) {- (?~)
                                                  (^.)
                                                , (^?)
                                                , (&)
                                                , (.~)
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
              (renderList (const $ playlistRow st PlaylistLeft)
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

playlistRow :: HState -> Name -> T.Text -> Widget n
playlistRow _ name val =
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
{-
playlistMoveRight :: FocLib -> FocLib
playlistMoveRight FocArtists = FocAlbums
playlistMoveRight _          = FocSongs

playlistMoveLeft :: FocLib -> FocLib
playlistMoveLeft FocSongs = FocAlbums
playlistMoveLeft _        = FocArtists

songBulkAdd :: Bool -> V.Vector MPD.Song -> HState -> EventM n (Next HState)
songBulkAdd play songs s = do
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
  continue s { currentSong = fromRight Nothing song, queue = queue s }

playlistAdd :: Bool -> HState -> EventM Name (Next HState)
playlistAdd play s =
  let libfoc = s ^. focusL . focPlayL
  in  case libfoc of
        FocPlaylists -> do
          songs <- liftIO
            (songsOfArtist (snd <$> listSelectedElement (artists s)))
          songBulkAdd play songs s
        FocPSongs -> do
          let maybeFilePath =
                MPD.sgFilePath . snd <$> listSelectedElement (songs s)
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

handleEventLibrary
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventLibrary s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> libraryMove listMoveDown s
    EvKey (KChar 'k') [] -> libraryMove listMoveUp s
    EvKey (KChar 'l') [] -> do
      continue $ s & focusL . focLibL %~ libraryMoveRight
    EvKey (KChar 'h') [] -> do
      continue $ s & focusL . focLibL %~ libraryMoveLeft
    EvKey KEnter      [] -> libraryAdd True s
    EvKey (KChar ' ') [] -> libraryAdd False s
    EvKey (KChar 'G') [] -> libraryMove (listMoveTo (length . queue $ s)) s
    EvKey (KChar 'g') [] -> libraryMove (listMoveTo 0) s -- TODO change this to  'gg', somehow
    _                    -> continue s
  _ -> continue s
-}
