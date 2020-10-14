-- |

module Ham.Modes.SongMode where
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border


drawSongModeHelp :: Widget n
drawSongModeHelp =
  centerLayer
    . borderWithLabel (txt "Change playback Mode")
    $ txt
        " s          single \n c          consume \n x          crossfade \n r          repeat \n R          random "
