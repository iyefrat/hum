-- |

module Types where
import           Network.MPD                   as MPD
import           Brick.Types
import           Brick.Widgets.List
import           Data.Time                      ( UTCTime )
data HState =
  HState { status :: Maybe MPD.Status
           ,currentSong :: Maybe MPD.Song
           ,playlist :: [ MPD.Song ]
           ,queue :: SongList
           ,queueExtent :: Maybe (Extent Name)
           ,clipboard :: SongList
           ,currentTime :: UTCTime
           }
  deriving (Show) --, Eq)

data Name = Queue | Queue0 | Clipboard
 deriving (Show, Eq, Ord)

type Highlight = Bool
type SongList = List Name (Song, Highlight)

data Tick = Tick
type HamEvent = Either Tick (Response [Subsystem])
