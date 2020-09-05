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
drawLibraryLeft st = error "whom"

drawViewLibrary :: HState -> [Widget Name]
drawViewLibrary st =
  [center $ withAttr queueArtistAttr $ txt "wait where are all the books"]

handleEventLibrary
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleEventLibrary s e = do
  continue s
