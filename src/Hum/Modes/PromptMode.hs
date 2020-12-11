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
import qualified Data.Vector                   as V
import Brick.Widgets.List


handlePromptEvent
    :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handlePromptEvent s e = case s ^. promptsL . currentPromptL of
  PlSelectPrompt -> handlePlSelectPromptEvent s e
  TextPrompt -> handleTextPromptEvent s e
  YNPrompt -> handleYNPromptEvent s e

handlePlSelectPromptEvent
    :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handlePlSelectPromptEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey KEsc [] -> continue $ s & modeL .~ NormalMode
    EvKey (KChar 'q') [] -> continue $ s & modeL .~ NormalMode
    EvKey (KChar 'j') [] -> do
      continue $ s & promptsL . plSelectPromptL %~ listMoveDown
    EvKey (KChar 'k') [] -> do
      continue $ s & promptsL . plSelectPromptL %~ listMoveUp
    EvKey (KChar 'l') [] -> do
      let songs = s ^. queueL & getHighlighted <&> fst & listElements
      case s ^. promptsL . plSelectPromptL & listSelectedElement <&> snd & join of
        Nothing -> continue $ s & promptsL . currentPromptL .~ TextPrompt
                                & promptsL . exitPromptL .~ songBulkAddtoNewPl songs
        Just plname -> continue =<< songBulkAddtoPl (MPD.toString plname) songs s
    _ -> continue s
  _ -> continue s

songBulkAddtoNewPl :: V.Vector MPD.Song -> HState -> EventM n HState
songBulkAddtoNewPl songs st = songBulkAddtoPl
  (toString $ st ^. promptsL . textPromptL . editContentsL & Z.currentLine)
  songs
  st

handleTextPromptEvent
    :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleTextPromptEvent s e = case e of
    VtyEvent (EvKey KEsc []) -> continue (s & modeL .~ NormalMode)
    VtyEvent (EvKey KEnter []) -> continue =<< (s ^. promptsL . exitPromptL) (s & modeL .~ NormalMode)
    VtyEvent vtye ->
        continue =<< handleEventLensed s (promptsL . textPromptL) handleEditorEvent vtye
    _ -> continue s

-- TODO
handleYNPromptEvent
    :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleYNPromptEvent s e = continue s
