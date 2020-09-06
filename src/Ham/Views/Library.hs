-- |

module Ham.Views.Library where
import           Ham.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
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
                                    True
                                    (MPD.toText <$> artists st)
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
                                    True
                                    (meta "<no title>" MPD.Title <$> songs st)
              )
          )



libraryRow :: HState -> Name -> T.Text -> Widget n
libraryRow st name val =
  withAttr
      (case name of
        LibraryRight -> queueTitleAttr
        LibraryLeft  -> queueArtistAttr
        _            -> listAttr
      )
    $ column Nothing (Pad 1) Max
    $ txt
    $ val


drawViewLibrary :: HState -> Widget Name
drawViewLibrary st = drawLibraryLeft st <+> drawLibraryRight st


handleEventLibrary
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventLibrary s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> do
      let artists' = listMoveDown $ artists s
      songsVec <- liftIO
        (   V.fromList
        <$> fromRight []
        <$> (withMPD $ MPD.find
              (      MPD.AlbumArtist
              MPD.=? fromMaybe "" (snd <$> listSelectedElement artists')
              )
            )
        )
      let songs = list SongsList songsVec 1
      continue s { artists = artists', songsVec, songs }
    EvKey (KChar 'k') [] -> do
      let artists' = listMoveUp $ artists s
      songsVec <- liftIO
        (   V.fromList
        <$> fromRight []
        <$> (withMPD $ MPD.find
              (      MPD.AlbumArtist
              MPD.=? fromMaybe "" (snd <$> listSelectedElement artists')
              )
            )
        )
      let songs = list SongsList songsVec 1
      continue s { artists = artists', songsVec, songs }
    _ -> continue s
  _ -> continue s
