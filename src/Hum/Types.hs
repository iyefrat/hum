-- | Module    : Hum.Types
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- Types!

module Hum.Types where
import           Network.MPD
import           Brick.BChan
import           Brick.Types
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Hum.Orphans ( )

-- | Describes the state of the app.
data HumState = HumState
    { chan        :: !(BChan HumEvent) -- ^ The channel for MPD and time events
    , hview       :: !View -- ^ The current view: Queue, Library, etc.
    , status      :: !(Maybe Status) -- ^ MPD's status
    , mode        :: !Mode -- ^ Input mode
    , ex          :: !ExState -- ^ The state of the ex mode style prompt at the bottom
    , currentSong :: !(Maybe Song)
    , queue       :: !SongList -- ^ Also called the playlist in MPD
    , library     :: !LibraryState
    , playlists   :: !PlaylistsState
    , clipboard   :: !Clipboard
    , focus       :: !Focus -- ^ The current focus in each view
    , editable    :: !Bool -- ^ Whether the selected stored playlist is editable
    , prompts     :: !Prompts
    , help        :: !HelpState -- ^ Help View
    }


data LibraryState = LibraryState
    { artists     :: !(List Name Value) -- ^ All album artists
    , yalbums     :: !(List Name (Value,Value)) -- ^ Year-Album pairs of the selected artist
    , yalbumSort  :: !Bool -- ^ Toggle sort of yalbums between years and alphabeitcal order
    , songs       :: !(List Name Song) -- ^ Songs in selected album
    }

-- | Stored playlists
data PlaylistsState = PlaylistsState
    { plList  :: !(List Name PlaylistName) -- ^ List of stored playlists
    , plSongs :: !SongList -- ^ Songs in selected playlist
    }
data HelpState = HelpState
    { helpText      :: !Text -- ^ Contents of help screen
    , helpSearchInt :: !Int -- ^ number of focused search match
    }
-- | Specific mode in the bottom prompt
data ExSubMode =
    Cmd -- ^ Function commands
  | FSearch -- ^ Forward search
  | BSearch -- ^ Backwards search
    deriving (Show, Eq, Ord)

data ExState = ExState
    { exPrefix        :: !ExSubMode
    , exEditor        :: !(Editor Text Name)
    , searchDirection :: !Bool -- ^ Search direction of last search
    , searchHistory   :: ![Text]
    , cmdHistory      :: ![Text]
    }

data Prompts = Prompts
    { currentPrompt      :: !PromptType
    , promptTitle        :: Text
    , plSelectPrompt     :: !(List Name (Maybe PlaylistName)) -- ^ List to select playlist from
    , textPrompt         :: !(Editor Text Name) -- ^ Editor if needed
    , exitPromptFunc     :: Bool -> HumState -> EventM Name HumState -- ^ Executes on exit from prompt, True for execute and False for quit.
    }

data PromptType =
    PlSelectPrompt -- ^ Select playlist to add songs to
  | YNPrompt -- ^ General yes/no prompt
  | TextPrompt -- ^ General enter text and do stuff prompt
  deriving (Show,Eq)

data Clipboard = Clipboard { clSongs  :: !SongList -- ^ Last list of songs copied
                           , clPlName :: !(Maybe PlaylistName) -- ^ Last playlist name copied
                           }
-- | General input mode
data Mode =
    NormalMode -- ^ Vim normal mode style movement
  | ExMode -- ^ Type ex style commands or search
  | PromptMode -- ^ Interact with a prompt
  deriving (Show,Eq)

type Highlight = Bool

type SongList = List Name (Song, Highlight)

type HumEvent = Either Tick (Response [Subsystem])

-- | Brick widget names
data Name =
    NowPlaying | ClSongs
  | Queue | QueueList
  | Library | ArtistsList | LibraryLeft | AlbumsList | YalbumsList | LibraryMid | SongsList | LibraryRight
  | PlaylistList | PlaylistLeft | PlaylistSongs | PlaylistRight
  | Help
  | ExEditor
  | TextPromptEditor
 deriving (Show, Eq, Ord)

data Focus = Focus
    { focQueue :: FocQueue
    , focLib   :: FocLib
    , focPlay  :: FocPlay
    , focEx    :: Bool
    }
    deriving (Show, Eq, Ord)

data FocQueue = FocQueue
    deriving (Show, Eq, Ord)

data FocLib = FocArtists | FocAlbums | FocSongs
  deriving(Show,Eq,Ord,Enum)

data FocPlay = FocPlaylists | FocPSongs
  deriving(Show,Eq,Ord,Enum)

data View = QueueView | LibraryView | PlaylistsView | HelpView
 deriving (Show,Eq,Ord)

data Tick = Tick

suffixLenses ''HumState
suffixLenses ''Focus
suffixLenses ''LibraryState
suffixLenses ''PlaylistsState
suffixLenses ''HelpState
suffixLenses ''ExState
suffixLenses ''Prompts
suffixLenses ''Clipboard
