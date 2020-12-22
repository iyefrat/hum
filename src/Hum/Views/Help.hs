-- |

module Hum.Views.Help where
import           Prelude                 hiding ( Down )
import           Hum.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Control.Lens


drawViewHelp :: HState -> Widget Name
drawViewHelp st = center ((hCenter . txt $ "j/k cycle between help screens.") <=> txt (helpText st))

helpText :: HState -> Text
helpText st = unlines $ case st^.helpScreenL of
  0 -> [
          "Change views:"
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
        , "  n and N - move to next and previous match of search"
        , "  :       - execute commands"
        , "  q       - quit"
        , "  s       - toggle single mode in mpd"
        , "  c       - toggle consume mode in mpd"
        , "  x       - toggle crossfade mode in mpd"
        , "  r       - toggle repeat mode in mpd"
        , "  z       - toggle random mode in mpd"]
  1 -> [
          "Queue keybindings:"
        , "  SPC - select song"
        , "  y and d - yank and delete the selected songs"
        , "  p   - paste selected song"
        , "  a   - add selected songs to playlist"
        , ""
        , "Library and Playlists keybindigns:"
        , "  SPC - add song/song collection to queue"
        , "  RET - add song/song collection to queue, and start playing the first one"
        , "  `   - toggle sort of the album column between release order and alphabetical order"
        , ""
        , "Playlists keybindigns:"
        , " on playlist conents:"
        , "  e - make playlist editable, press again to get save prompt."
        , "      editing a playlist is the same as editing the queue"
        , " on list of playlists:"
        , "  y and p - copy and paste playlists (with -copy added to the name)"
        , "  d       - delete playlist (with prompt)"
        , ""
        , "Commands:"
        , ":help       - gets you this"
        , ":q          - quits"
        , ":save $name - saves the queue to a playlist called $name"
        ]
  _ -> ["something went wrong."]


handleEventHelp
  :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleEventHelp s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> continue $ s & helpScreenL %~ (\x -> if x==1 then 0 else x+1)
    EvKey (KChar 'k') [] -> continue $ s & helpScreenL %~ (\x -> if x==0 then 1 else x-1)
    EvKey (KChar 'n') [] -> continue s
    EvKey (KChar 'N') [] -> continue s
    EvKey (KChar 'G') [] -> continue s
    EvKey (KChar 'g') [] -> continue s
    _                    -> continue s
  _ -> continue s
