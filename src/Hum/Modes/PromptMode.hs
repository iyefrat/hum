
-- | Module    : Hum.Modes.PromptMode
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- Functions for prompts.


module Hum.Modes.PromptMode where

import           Brick.Widgets.Edit      hiding ( decodeUtf8 )
import           Brick.Widgets.List
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

-- | Prompt key event dispatch.
handlePromptEvent
    :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handlePromptEvent s e = case s ^. promptsL . currentPromptL of
  PlSelectPrompt -> handlePlSelectPromptEvent s e
  TextPrompt -> handleTextPromptEvent s e
  YNPrompt -> handleYNPromptEvent s e

-- | Handles key events for playlist select prompt.
handlePlSelectPromptEvent
    :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handlePlSelectPromptEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey KEsc [] -> continue
        =<< (s ^. promptsL . exitPromptFuncL) False (s & modeL .~ NormalMode)
    EvKey (KChar 'q') [] -> continue
      =<< (s ^. promptsL . exitPromptFuncL) False (s & modeL .~ NormalMode)
    EvKey (KChar 'j') [] -> do
      continue $ s & promptsL . plSelectPromptL %~ listMoveDown
    EvKey (KChar 'k') [] -> do
      continue $ s & promptsL . plSelectPromptL %~ listMoveUp
    EvKey (KChar 'l') [] -> do
      let songs = s ^. queueL & getHighlighted <&> fst & listElements
      case s ^. promptsL . plSelectPromptL & listSelectedElement <&> snd & join of
        Nothing -> continue $ s & promptsL . currentPromptL .~ TextPrompt
                                & promptsL . exitPromptFuncL .~ songBulkAddtoNewPl songs
        Just plname -> continue =<< songBulkAddtoPl (MPD.toString plname) songs s
    EvKey KEnter [] -> do -- HACK find a way to unduplucate this
      let songs = s ^. queueL & getHighlighted <&> fst & listElements
      case s ^. promptsL . plSelectPromptL & listSelectedElement <&> snd & join of
        Nothing -> continue $ s & promptsL . currentPromptL .~ TextPrompt
                                & promptsL . exitPromptFuncL .~ songBulkAddtoNewPl songs
        Just plname -> continue =<< songBulkAddtoPl (MPD.toString plname) songs s
    _ -> continue s
  _ -> continue s

-- | Add given songs to new playlist entered in prompt.
songBulkAddtoNewPl
  :: V.Vector MPD.Song -> Bool -> HumState -> EventM n HumState
songBulkAddtoNewPl songs bl st = if bl
  then songBulkAddtoPl
    (toString $ st ^. promptsL . textPromptL . editContentsL & Z.currentLine)
    songs
    st
  else pure st

-- | Handles key events for generic text prompt.
handleTextPromptEvent
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleTextPromptEvent s e = case e of
  VtyEvent (EvKey KEsc []) -> continue
    =<< (s ^. promptsL . exitPromptFuncL) False (s & modeL .~ NormalMode)
  VtyEvent (EvKey KEnter []) -> continue
    =<< (s ^. promptsL . exitPromptFuncL) True (s & modeL .~ NormalMode)
  VtyEvent vtye ->
    continue
      =<< handleEventLensed s (promptsL . textPromptL) handleEditorEvent vtye
  _ -> continue s

-- | Handles key events for generic yes/no prompt.
handleYNPromptEvent
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleYNPromptEvent s e = case e of
  VtyEvent (EvKey KEsc []) ->
    continue
      =<< (s ^. promptsL . exitPromptFuncL) False (s & modeL .~ NormalMode)
  VtyEvent (EvKey (KChar 'y') []) ->
    continue
      =<< (s ^. promptsL . exitPromptFuncL) True (s & modeL .~ NormalMode)
  VtyEvent (EvKey (KChar 'n') []) ->
    continue
      =<< (s ^. promptsL . exitPromptFuncL) False (s & modeL .~ NormalMode)
  VtyEvent (EvKey (KChar 'q') []) ->
    continue
      =<< (s ^. promptsL . exitPromptFuncL) False (s & modeL .~ NormalMode)
  _                               -> continue s
