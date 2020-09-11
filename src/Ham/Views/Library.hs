-- |

module Ham.Views.Library where
import           Prelude                 hiding ( Down )
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



drawLibraryLeft :: HState -> Widget Name
drawLibraryLeft st =
  let vsize = case join $ Map.lookup LibraryLeft $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent LibraryLeft $ hCenter
        (   viewport LibraryLeft Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ libraryRow st LibraryLeft)
                          ((focLib . focus $ st) == FocArtists)
                          (MPD.toText <$> artists st)
              )
        )
drawLibraryMid :: HState -> Widget Name
drawLibraryMid st =
  let vsize = case join $ Map.lookup LibraryMid $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent LibraryMid $ hCenter
        (   viewport LibraryMid Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ libraryRow st LibraryMid)
                          ((focLib . focus $ st) == FocAlbums)
                          (MPD.toText <$> albums st)
              )
        )


drawLibraryRight :: HState -> Widget Name
drawLibraryRight st =
  let vsize = case join $ Map.lookup LibraryRight $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent LibraryRight $ hCenter
        (   viewport LibraryRight Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ librarySongRow st)
                          ((focLib . focus $ st) == FocSongs)
                          (songs st)
              )
        )


libraryRow :: HState -> Name -> T.Text -> Widget n
libraryRow _ name val =
  withAttr
      (case name of
        LibraryLeft -> queueArtistAttr
        LibraryMid  -> queueAlbumAttr
        _           -> listAttr
      )
    $ column Nothing (Pad 1) Max
    $ txt val

librarySongRow :: HState -> MPD.Song -> Widget n
librarySongRow st song =
  let pathsInQueue =
          (MPD.sgFilePath <$>) . (fst <$>) . listElements . queue $ st
  in  withAttr
          (if MPD.sgFilePath song `elem` pathsInQueue
            then queueTitleBoldAttr
            else queueTitleAttr
          )
        $ column Nothing (Pad 1) Max
        $ txt (meta (MPD.toText . MPD.sgFilePath $ song) MPD.Title song)

libraryMoveRight :: FocLib -> FocLib
libraryMoveRight FocArtists = FocAlbums
libraryMoveRight _          = FocSongs

libraryMoveLeft :: FocLib -> FocLib
libraryMoveLeft FocSongs = FocAlbums
libraryMoveLeft _        = FocArtists

drawViewLibrary :: HState -> Widget Name
drawViewLibrary st =
  drawLibraryLeft st <+> drawLibraryMid st <+> drawLibraryRight st

libraryMove :: Direction -> HState -> EventM Name (Next HState)
libraryMove direction s =
  let libfoc   = s ^. focusL . focLibL
      listMove = case direction of
        Up   -> listMoveUp
        Down -> listMoveDown
  in  case libfoc of
        FocArtists -> do
          let artists' = listMove $ artists s
          albumsVec <- liftIO
            (albumsOfArtist (snd <$> listSelectedElement artists'))
          let albums = list AlbumsList albumsVec 1
          songsVec <- liftIO (songsOfAlbum (snd <$> listSelectedElement albums))
          let songs = list SongsList songsVec 1
          continue s { artists = artists', songs, albums }
        FocAlbums -> do
          let albums' = listMove $ albums s
          songsVec <- liftIO
            (songsOfAlbum (snd <$> listSelectedElement albums'))
          let songs = list SongsList songsVec 1
          continue s { songs, albums = albums' }
        FocSongs -> do
          let songs' = listMove $ songs s
          continue s { songs = songs' }

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

libraryAdd :: Bool -> HState -> EventM Name (Next HState)
libraryAdd play s =
  let libfoc = s ^. focusL . focLibL
  in  case libfoc of
        FocArtists -> do
          songs <- liftIO
            (songsOfArtist (snd <$> listSelectedElement (artists s)))
          songBulkAdd play songs s
        FocAlbums -> do
          songs <- liftIO
            (songsOfAlbum (snd <$> listSelectedElement (albums s)))
          songBulkAdd play songs s
        FocSongs -> do
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
    EvKey (KChar 'j') [] -> libraryMove Down s
    EvKey (KChar 'k') [] -> libraryMove Up s
    EvKey (KChar 'l') [] -> do
      continue $ s & focusL . focLibL %~ libraryMoveRight
    EvKey (KChar 'h') [] -> do
      continue $ s & focusL . focLibL %~ libraryMoveLeft
    EvKey KEnter      [] -> libraryAdd True s
    EvKey (KChar ' ') [] -> libraryAdd False s
    _                    -> continue s
  _ -> continue s
