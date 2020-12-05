-- |

module Hum.Modes.PromptMode where

import           Brick.Widgets.Edit      hiding ( decodeUtf8 )
import           Brick.Types
import           Brick.Main
import           Hum.Types
import           Hum.Views
import           Hum.Utils
import           Hum.Rebuild
import           Graphics.Vty.Input.Events
import qualified Data.Text.Zipper              as Z
                                         hiding ( textZipper )
import           Lens.Micro
import qualified Network.MPD                   as MPD

handlePromptEvent
    :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handlePromptEvent s e = case e of
    VtyEvent (EvKey KEsc []) -> continue $ s & modeL .~ NormalMode
    VtyEvent vtye ->
        continue =<< handleEventLensed s (exL . exEditorL) handleEditorEvent vtye
    _ -> continue s
