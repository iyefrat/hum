-- |

module Ham.Types where
import           Network.MPD                   as MPD
import qualified Brick.BChan                   as BC
import           Brick.Types
import           Brick.Widgets.List
import           Data.Time                      ( UTCTime )


data HState =
  HState { chan :: !(BC.BChan HamEvent)
           ,view :: !View
           ,status :: !(Maybe MPD.Status)
           ,mode :: !Mode
           ,currentSong :: !(Maybe MPD.Song)
           ,queue :: !SongList
           ,extentMap :: !(Map Name (Maybe (Extent Name)))
           ,clipboard :: !SongList
           ,currentTime :: !UTCTime
           ,artists :: !(List Name Value)
           ,albums :: !(List Name Value)
           ,songs :: !(List Name Song)
           ,focus :: !Focus
           ,playlists :: !(List Name PlaylistName)
           ,playlistSongs :: !(List Name Song)
           }
--  deriving (Show) --, Eq)

data Mode = NormalMode | ExMode | SearchMode | SongModeMode
type SongList = List Name (Song, Highlight)

type HamEvent = Either Tick (Response [Subsystem])

data Name = NowPlaying | Clipboard
  | Queue | QueueList
  | Library | ArtistsList | LibraryLeft | AlbumsList | LibraryMid| SongsList | LibraryRight
  | PlaylistList | PlaylistLeft | PlaylistSongs | PlaylistRight
 deriving (Show, Eq, Ord)

data FocQueue = FocQueue
  deriving(Show,Eq,Ord)
data FocLib = FocArtists | FocAlbums | FocSongs
  deriving(Show,Eq,Ord,Enum)
data FocPlay = FocPlaylists | FocPSongs
  deriving(Show,Eq,Ord,Enum)
data Focus = Focus { focQueue :: FocQueue
                     ,focLib :: FocLib
                     ,focPlay :: FocPlay}
  deriving(Show,Eq,Ord)

data View = QueueView | LibraryView | PlaylistsView
 deriving (Show,Eq,Ord)

type Highlight = Bool

data Tick = Tick

suffixLenses ''HState
suffixLenses ''Focus
