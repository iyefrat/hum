-- |

module Ham.Views.Library where
import           Ham.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Lens.Micro                     ( (?~)
                                                , (^.)
                                                , (^?)
--                                                , (&)
                                                , (.~)
                                                , (%~)
                                                , _2
                                                , _head
                                                , set
                                                )

import           Brick.Widgets.List
import           Ham.Song
import           Ham.Attributes
import           Ham.Views.Common
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Ham.Utils



drawLibraryLeft :: HState -> Widget Name
drawLibraryLeft st =
  let vsize = case (join $ Map.lookup LibraryLeft $ extentMap st) of
        Just e  -> (snd . extentSize $ e)
        Nothing -> 20
  in  reportExtent LibraryLeft
        $ hCenter
        $ (   viewport LibraryLeft Vertical
          .   visible
          .   vLimit vsize
          .   center
          $   hBorder
          <=> (hCenter $ renderList (const $ libraryRow st LibraryLeft)
                                    ((focLib . focus $ st) == FocArtists)
                                    (MPD.toText <$> artists st)
              )
          )
drawLibraryMid :: HState -> Widget Name
drawLibraryMid st =
  let vsize = case (join $ Map.lookup LibraryMid $ extentMap st) of
        Just e  -> (snd . extentSize $ e)
        Nothing -> 20
  in  reportExtent LibraryMid
        $ hCenter
        $ (   viewport LibraryMid Vertical
          .   visible
          .   vLimit vsize
          .   center
          $   hBorder
          <=> (hCenter $ renderList (const $ libraryRow st LibraryMid)
                                    ((focLib . focus $ st) == FocAlbums)
                                    (MPD.toText <$> albums st)
              )
          )


drawLibraryRight :: HState -> Widget Name
drawLibraryRight st =
  let vsize = case (join $ Map.lookup LibraryRight $ extentMap st) of
        Just e  -> (snd . extentSize $ e)
        Nothing -> 20
  in  reportExtent LibraryRight
        $ hCenter
        $ (   viewport LibraryRight Vertical
          .   visible
          .   vLimit vsize
          .   center
          $   hBorder
          <=> (hCenter $ renderList (const $ libraryRow st LibraryRight)
                                    ((focLib . focus $ st) == FocSongs)
                                    (meta "<no title>" MPD.Title <$> songs st)
              )
          )



libraryRow :: HState -> Name -> T.Text -> Widget n
libraryRow st name val =
  withAttr
      (case name of
        LibraryRight -> queueTitleAttr
        LibraryMid   -> queueAlbumAttr
        LibraryLeft  -> queueArtistAttr
        _            -> listAttr
      )
    $ column Nothing (Pad 1) Max
    $ txt
    $ val

libraryMoveRight :: FocLib -> FocLib
libraryMoveRight FocArtists = FocAlbums
libraryMoveRight _          = FocSongs

libraryMoveLeft :: FocLib -> FocLib
libraryMoveLeft FocSongs = FocAlbums
libraryMoveLeft _        = FocArtists

drawViewLibrary :: HState -> Widget Name
drawViewLibrary st =
  drawLibraryLeft st <+> drawLibraryMid st <+> drawLibraryRight st


handleEventLibrary
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventLibrary s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> do
      let artists' = listMoveDown $ artists s
      albumsVec <- liftIO
        (albumsOfArtist (snd <$> listSelectedElement artists'))
      let albums = list AlbumsList albumsVec 1
      songsVec <- liftIO (songsOfArtist (snd <$> listSelectedElement artists'))
      let songs = list SongsList songsVec 1
      continue s { artists = artists', songs, albums }
    EvKey (KChar 'k') [] -> do
      let artists' = listMoveUp $ artists s
      albumsVec <- liftIO
        (albumsOfArtist (snd <$> listSelectedElement artists'))
      let albums = list AlbumsList albumsVec 1
      songsVec <- liftIO (songsOfArtist (snd <$> listSelectedElement artists'))
      let songs = list SongsList songsVec 1
      continue s { artists = artists', songs, albums }
    EvKey (KChar 'l') [] -> do
      continue $ s & focusL . focLibL %~ libraryMoveRight
    EvKey (KChar 'h') [] -> do
      continue $ s & focusL . focLibL %~ libraryMoveLeft
    _ -> continue s
  _ -> continue s
