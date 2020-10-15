-- |

module Hmm.Modes.SearchMode where

import           Brick.Widgets.Edit      hiding ( decodeUtf8 )
import qualified Brick.Widgets.Edit            as E
                                                ( decodeUtf8 )
import           Brick.Types
import           Brick.Main
import           Hmm.Types
import           Graphics.Vty.Input.Events
import qualified Data.Text.Zipper              as Z
                                         hiding ( textZipper )
import qualified Data.Text.Zipper.Generic      as Z
import           Lens.Micro                     ( (?~)
                                                , (^.)
                                                , (^?)
                                                , (.~)
                                                , (%~)
                                                , _2
                                                , _head
                                                , set
                                                )

handleSearchEvent
  :: HState -> BrickEvent Name HmmEvent -> EventM Name (Next HState)
handleSearchEvent s e = case e of
  VtyEvent (EvKey KEnter []) ->
    continue
      $  s
      &  searchHistoryL
      %~ ((s ^. searchL . editContentsL & Z.currentLine) :)
      &  modeL
      .~ NormalMode
      &  focusL
      .  focSearchL
      .~ False
  VtyEvent vtye ->
    continue =<< handleEventLensed s searchL handleEditorEvent vtye
  _ -> continue s
