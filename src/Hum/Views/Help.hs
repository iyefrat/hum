{-#LANGUAGE RankNTypes#-}
-- |

module Hum.Views.Help where
import           Prelude                 hiding ( Down )
import           Hum.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Lens.Micro
import           Brick.Widgets.List
import           Hum.Song
import           Hum.Attributes
import           Hum.Views.Common
import           Hum.Rebuild
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Hum.Utils
import qualified Data.Text.Zipper              as Z


drawViewHelp :: Widget Name
drawViewHelp = center $ txt helpText

helpText :: Text
helpText = unlines
  [ "Greetings hum user. Here are the commands and keybindings:"
  , ""
  , "Change views:"
  , "  1 - queue"
  , "  2 - library"
  , "  3 - playlists"
  , ""
  , "General Bindings:"
  , "  t       - play/pause toggle"
  , "  hjkl    - vim movements"
  , "  / and ? - forwards and backwards search"
  , "  n and N - move to next match of search"
  , "  :       - execute commands"
  , "  q       - quit"
  , "  s       - toggle single mode in mpd"
  , "  c       - toggle consume mode in mpd"
  , "  x       - toggle crossfade mode in mpd"
  , "  r       - toggle repeat mode in mpd"
  , "  z       - toggle random mode in mpd"
  , ""
  , "Queue keybindings:"
  , "  SPC - select song"
  , "  y/d - yank/delete the selected songs"
  , "  p   - paste selected song"
  , ""
  , "Library and Playlists keybindigns:"
  , "  SPC - add song/song collection to queue"
  , "  RET - add song/song collection to queue, and start playing the first one"
  , ""
  , "Commands:"
  , ":help - gets you here"
  , ":q    - quits"
  ]

handleEventHelp
  :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleEventHelp s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> continue s
    EvKey (KChar 'k') [] -> continue s
    EvKey (KChar 'n') [] -> continue s
    EvKey (KChar 'N') [] -> continue s
    EvKey (KChar 'G') [] -> continue s
    EvKey (KChar 'g') [] -> continue s
    _                    -> continue s
  _ -> continue s
