-- |

module Hum.Types where
import           Network.MPD                   as MPD
import qualified Brick.BChan                   as BC
import           Brick.Types
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Hum.Orphans()


data HState = HState
    { chan        :: !(BC.BChan HumEvent)
    , hview        :: !View
    , status      :: !(Maybe MPD.Status)
    , mode        :: !Mode
    , ex          :: !ExState
    , currentSong :: !(Maybe MPD.Song)
    , queue       :: !SongList
    , library     :: !LibraryState
    , playlists   :: !PlaylistsState
    , clipboard   :: !Clipboard
    , focus       :: !Focus
    , editable    :: !Bool
    , prompts     :: !Prompts
    , helpScreen  :: !Int --HACK
    }
--  deriving (Show) --, Eq)

data LibraryState = LibraryState
    { artists :: !(List Name MPD.Value)
    , albums  :: !(List Name MPD.Value)
    , songs   :: !(List Name Song)
    }

data PlaylistsState = PlaylistsState
    { plList  :: !(List Name PlaylistName)
    , plSongs :: !SongList
    }

data ExSubMode = Cmd | FSearch | BSearch
    deriving (Show, Eq, Ord)

data ExState = ExState
    { exPrefix        :: !ExSubMode
    , exEditor        :: !(Editor Text Name)
    , searchDirection :: !Bool
    , searchHistory   :: ![Text]
    , cmdHistory      :: ![Text]
    }

data Prompts = Prompts
    { currentPrompt  :: !PromptType
    , promptTitle    :: Text
    , plSelectPrompt :: !(List Name (Maybe PlaylistName))
    , textPrompt     :: !(Editor Text Name)
    , exitPrompt     :: HState -> EventM Name HState
    }

data PromptType = PlSelectPrompt | YNPrompt | TextPrompt
  deriving (Show,Eq)

data Clipboard = Clipboard { clSongs  :: !SongList
                           , clPlName :: !(Maybe PlaylistName)}

data Mode = NormalMode | ExMode | PromptMode
  deriving (Show,Eq)

type SongList = List Name (Song, Highlight)

type HumEvent = Either Tick (Response [Subsystem])

data Name = NowPlaying | ClSongs
  | Queue | QueueList
  | Library | ArtistsList | LibraryLeft | AlbumsList | LibraryMid | SongsList | LibraryRight
  | PlaylistList | PlaylistLeft | PlaylistSongs | PlaylistRight
  | ExEditor
  | TextPromptEditor
 deriving (Show, Eq, Ord)

data FocQueue = FocQueue
    deriving (Show, Eq, Ord)
data FocLib = FocArtists | FocAlbums | FocSongs
  deriving(Show,Eq,Ord,Enum)
data FocPlay = FocPlaylists | FocPSongs
  deriving(Show,Eq,Ord,Enum)
data Focus = Focus
    { focQueue :: FocQueue
    , focLib   :: FocLib
    , focPlay  :: FocPlay
    , focEx    :: Bool
    }
    deriving (Show, Eq, Ord)

data View = QueueView | LibraryView | PlaylistsView | HelpView
 deriving (Show,Eq,Ord)

type Highlight = Bool

data Tick = Tick

suffixLenses ''HState
suffixLenses ''Focus
suffixLenses ''LibraryState
suffixLenses ''PlaylistsState
suffixLenses ''ExState
suffixLenses ''Prompts
suffixLenses ''Clipboard
