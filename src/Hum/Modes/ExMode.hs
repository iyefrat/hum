-- |

module Hum.Modes.ExMode where

import           Brick.Widgets.Edit      hiding ( decodeUtf8 )
import qualified Brick.Widgets.Edit            as E
                                                ( decodeUtf8 )
import           Brick.Types
import           Brick.Main
import           Hum.Types
import           Hum.Views
import           Graphics.Vty.Input.Events
import qualified Data.Text                     as T
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

searchEnd :: HState -> EventM Name (Next HState)
searchEnd s =
    let searched = (s ^. exL . exEditorL . editContentsL & Z.currentLine)
        s'= s & exL . searchHistoryL %~ (searched :)
              & exL . exEditorL .~ editorText ExEditor (Just 1) ""
              & modeL .~ NormalMode
              & focusL . focExL .~ False
    in  case view s' of
                QueueView     -> queueSearch True s'
                LibraryView   -> librarySearch True s'
                PlaylistsView -> playlistsSearch True s'

handleSearchEvent
    :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleSearchEvent s e = case e of
    VtyEvent (EvKey KEnter []) -> searchEnd s
    VtyEvent vtye ->
        continue =<< handleEventLensed s (exL . exEditorL) handleEditorEvent vtye
    _ -> continue s
