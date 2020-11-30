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
  , "  ,       - previous song"
  , "  .       - next song"
  , "  [ and ] - skip 5 second in either direction"
  , "  { and } - skip 30 second in either direction"
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
  , "  y and d - yank and delete the selected songs"
  , "  p   - paste selected song"
  , ""
  , "Library and Playlists keybindigns:"
  , "  SPC - add song/song collection to queue"
  , "  RET - add song/song collection to queue, and start playing the first one"
  , ""
  , "Commands:"
  , ":help       - gets you this"
  , ":q          - quits"
  , ":save $name - saves the queue to a playlist called $name"
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
