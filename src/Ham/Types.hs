-- |

module Ham.Types where
import           Network.MPD                   as MPD
import qualified Brick.BChan                   as BC
import           Brick.Types
import           Brick.Widgets.List
import           Data.Time                      ( UTCTime )
import qualified Data.Vector                   as V


data HState =
  HState { chan :: (BC.BChan HamEvent)
           ,view :: View
           ,status :: Maybe MPD.Status
           ,currentSong :: Maybe MPD.Song
           ,queueVec :: V.Vector MPD.Song
           ,queue :: SongList
           ,queueExtent :: Maybe (Extent Name)
           ,clipboard :: SongList
           ,currentTime :: UTCTime
           ,artistsVec :: V.Vector Value
           ,artists :: List Name Value
           ,focus :: Focus
           }
--  deriving (Show) --, Eq)

type SongList = List Name (Song, Highlight)

type HamEvent = Either Tick (Response [Subsystem])

data Name = Queue | QueueList | Clipboard | Library | ArtistsList | LibraryLeft
 deriving (Show, Eq, Ord)

data Focus = FocArtists | FocAlbums | FocSongs
 deriving(Show,Eq,Ord)
data View = QueueView | LibraryView
 deriving (Show,Eq,Ord)

type Highlight = Bool


data Tick = Tick
