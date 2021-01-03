-- |

module Hum.Modes.PromptMode where

import           Brick.Widgets.Edit      hiding ( decodeUtf8 )
import           Brick.Types
import           Brick.Main
import           Hum.Types
import           Hum.Utils
import           Graphics.Vty.Input.Events
import qualified Data.Text.Zipper              as Z
                                         hiding ( textZipper )
import           Control.Lens
import qualified Network.MPD                   as MPD
import qualified Data.Vector                   as V
import Brick.Widgets.List


handlePromptEvent
    :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handlePromptEvent s e = case s ^. promptsL . currentPromptL of
  PlSelectPrompt -> handlePlSelectPromptEvent s e
  TextPrompt -> handleTextPromptEvent s e
  YNPrompt -> handleYNPromptEvent s e

handlePlSelectPromptEvent
    :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
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
    EvKey KEnter [] -> do -- HACK find a way to unduplucate this
      let songs = s ^. queueL & getHighlighted <&> fst & listElements
      case s ^. promptsL . plSelectPromptL & listSelectedElement <&> snd & join of
        Nothing -> continue $ s & promptsL . currentPromptL .~ TextPrompt
                                & promptsL . exitPromptL .~ songBulkAddtoNewPl songs
        Just plname -> continue =<< songBulkAddtoPl (MPD.toString plname) songs s
    _ -> continue s
  _ -> continue s

songBulkAddtoNewPl :: V.Vector MPD.Song -> HumState -> EventM n HumState
songBulkAddtoNewPl songs st = songBulkAddtoPl
  (toString $ st ^. promptsL . textPromptL . editContentsL & Z.currentLine)
  songs
  st

handleTextPromptEvent
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleTextPromptEvent s e = case e of
  VtyEvent (EvKey KEsc []) -> continue (s & modeL .~ NormalMode)
  VtyEvent (EvKey KEnter []) ->
    continue =<< (s ^. promptsL . exitPromptL) (s & modeL .~ NormalMode)
  VtyEvent vtye ->
    continue
      =<< handleEventLensed s (promptsL . textPromptL) handleEditorEvent vtye
  _ -> continue s

handleYNPromptEvent
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleYNPromptEvent s e = case e of
  VtyEvent (EvKey KEsc []) -> continue (s & modeL .~ NormalMode)
  VtyEvent (EvKey (KChar 'y') []) ->
    continue =<< (s ^. promptsL . exitPromptL) (s & modeL .~ NormalMode)
  VtyEvent (EvKey (KChar 'n') []) -> continue (s & modeL .~ NormalMode)
  VtyEvent (EvKey (KChar 'q') []) -> continue (s & modeL .~ NormalMode)
  _                               -> continue s
