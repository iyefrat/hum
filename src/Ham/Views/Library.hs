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
import qualified Network.MPD                   as MPD


drawLibraryLeft :: HState -> Widget Name
drawLibraryLeft st =
  let vsize = case queueExtent st of
        Just e  -> (snd . extentSize $ e)
        Nothing -> 60
  in                               {-reportExtent LibraryLeft
        $ -}
      hCenter
        $ (   viewport LibraryLeft Vertical
          .   visible
          .   vLimit vsize
          .   center
          $   hBorder
          <=> (hCenter $ renderList (const $ libraryRow st) True (artists st))
          )

libraryRow :: HState -> MPD.Value -> Widget n
libraryRow st val = txt $ MPD.toText val


drawViewLibrary :: HState -> [Widget Name]
drawViewLibrary st =
  [ (withAttr queueArtistAttr $ txt "wait where are all the books")
      <=> drawLibraryLeft st
  ]

handleEventLibrary
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventLibrary s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> do
      continue s { artists = listMoveDown $ artists s }
    EvKey (KChar 'k') [] -> do
      continue s { artists = listMoveUp $ artists s }
    _ -> continue s
  _ -> continue s
