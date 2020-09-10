-- |

module Ham.Types where
import           Network.MPD                   as MPD
import qualified Brick.BChan                   as BC
import           Brick.Types
import           Brick.Widgets.List
import           Data.Time                      ( UTCTime )
import qualified Data.Vector                   as V
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map


data HState =
  HState { chan :: (BC.BChan HamEvent)
           ,view :: View
           ,status :: Maybe MPD.Status
           ,currentSong :: Maybe MPD.Song
           ,queue :: SongList
           ,extentMap :: Map Name (Maybe (Extent Name))
           ,clipboard :: SongList
           ,currentTime :: UTCTime
           ,artists :: List Name Value
           ,albums :: List Name Value
           ,songs :: List Name Song
           ,focus :: Focus
           }
--  deriving (Show) --, Eq)

type SongList = List Name (Song, Highlight)

type HamEvent = Either Tick (Response [Subsystem])

data Name = NowPlaying | Clipboard
  | Queue | QueueList
  | Library | ArtistsList | LibraryLeft | AlbumsList | LibraryMid| SongsList | LibraryRight
 deriving (Show, Eq, Ord)

data FocQueue = FocQueue
  deriving(Show,Eq,Ord)
data FocLib = FocArtists | FocAlbums | FocSongs
  deriving(Show,Eq,Ord,Enum)
data Focus = Focus { focQueue :: FocQueue
                     ,focLib :: FocLib}
  deriving(Show,Eq,Ord)

data View = QueueView | LibraryView
 deriving (Show,Eq,Ord)

type Highlight = Bool


data Tick = Tick

suffixLenses ''HState
suffixLenses ''Focus
