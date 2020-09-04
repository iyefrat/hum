-- |

module Types where
import           Network.MPD                   as MPD
import           Brick.Types
import           Brick.Widgets.List

data HState =
  HState { status :: Maybe MPD.Status
           ,currentSong :: Maybe MPD.Song
           ,playlist :: [ MPD.Song ]
           ,queue :: SongList
           ,queueExtent :: Maybe (Extent Name)
           ,clipboard :: SongList
           }
  deriving (Show) --, Eq)

data Name = Queue | Queue0 | Clipboard
 deriving (Show, Eq, Ord)

type Highlight = Bool
type SongList = List Name (Song, Highlight)
