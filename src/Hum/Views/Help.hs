
-- | Module    : Hum.Views.Help
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- Shared functions for views.


module Hum.Views.Help where
import           Prelude                 hiding ( Down )
import           Hum.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Control.Lens

-- | Draws help.
drawViewHelp :: HumState -> Widget Name
drawViewHelp st = viewport Help Vertical (txt (st ^. helpL . helpTextL))

-- | Helper function that keeps "Hum.UI" tidy.
helpText' :: Text
helpText' = unlines
        [
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
        , "  z       - toggle random mode in mpd"
        , ""
        , "Queue keybindings:"
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

-- | handle key events in help view.
handleEventHelp
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleEventHelp s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> vScrollBy (viewportScroll Help) 1 >> continue s
    EvKey (KChar 'k') [] -> vScrollBy (viewportScroll Help) (-1) >> continue s
    EvKey (KChar 'n') [] -> continue s
    EvKey (KChar 'N') [] -> continue s
    EvKey (KChar 'G') [] -> continue s
    EvKey (KChar 'g') [] -> continue s
    _                    -> continue s
  _ -> continue s
  where helpHeight = length . lines $ s ^. helpL . helpTextL
