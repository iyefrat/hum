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

exEnd :: HState -> EventM Name (Next HState)
exEnd s =
    let searched = (s ^. exL . exEditorL . editContentsL & Z.currentLine)
        s'= s & exL . exEditorL .~ editorText ExEditor (Just 1) ""
              & modeL .~ NormalMode
              & focusL . focExL .~ False
    in
          case s ^. exL . exPrefixL of
            Cmd -> exCmdExecute searched (s' & exL . cmdHistoryL %~ (searched :) )
            srch -> case view s' of
                QueueView     -> continue =<< queueSearch (srch == FSearch) s''
                LibraryView   -> continue =<< librarySearch (srch == FSearch) s''
                PlaylistsView -> continue =<< playlistsSearch (srch == FSearch) s''
                HelpView      -> continue s'
                where s''= s' & exL . searchHistoryL %~ (searched :)
handleExEvent
    :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
handleExEvent s e = case e of
    VtyEvent (EvKey KEnter []) -> exEnd s
    VtyEvent vtye ->
        continue =<< handleEventLensed s (exL . exEditorL) handleEditorEvent vtye
    _ -> continue s

exPrefixTxt :: ExSubMode -> Text
exPrefixTxt Cmd = ":"
exPrefixTxt FSearch= "/"
exPrefixTxt BSearch = "?"

exCmdExecute :: Text -> HState -> EventM Name (Next HState)
exCmdExecute "help" s = continue s { view = HelpView }
exCmdExecute "q" s = halt s
exCmdExecute _ s = continue s
