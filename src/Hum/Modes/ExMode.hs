-- |

module Hum.Modes.ExMode where

import           Brick.Widgets.Edit      hiding ( decodeUtf8 )
import           Brick.Types
import           Brick.Main
import           Hum.Types
import           Hum.Views
import           Hum.Rebuild
import           Graphics.Vty.Input.Events
import qualified Data.Text.Zipper              as Z
import qualified Data.Text                     as T
import           Control.Lens hiding (uncons)
import qualified Network.MPD                   as MPD

exEnd :: HumState -> EventM Name (Next HumState)
exEnd s =
    let searched = (s ^. exL . exEditorL . editContentsL & Z.currentLine)
        s'= s & exL . exEditorL .~ editorText ExEditor (Just 1) ""
              & modeL .~ NormalMode
              & focusL . focExL .~ False
    in
          case s ^. exL . exPrefixL of
            Cmd -> exCmdExecute (words searched) (s' & exL . cmdHistoryL %~ (searched :) )
            srch -> case hview s' of
                QueueView     -> continue =<< queueSearch (srch == FSearch) s''
                LibraryView   -> continue =<< librarySearch (srch == FSearch) s''
                PlaylistsView -> continue =<< playlistsSearch (srch == FSearch) s''
                HelpView      -> continue s'
                where s''= s' & exL . searchHistoryL %~ (searched :)
handleExEvent
    :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleExEvent s e = case e of
    VtyEvent (EvKey KEsc []) ->
        continue $ s & exL . exEditorL .~ editorText ExEditor (Just 1) ""
                     & modeL .~ NormalMode
                     & focusL . focExL .~ False
    VtyEvent (EvKey KEnter []) -> exEnd s
    VtyEvent vtye ->
        continue =<< handleEventLensed s (exL . exEditorL) handleEditorEvent vtye
    _ -> continue s

exPrefixTxt :: ExSubMode -> Text
exPrefixTxt Cmd = ":"
exPrefixTxt FSearch= "/"
exPrefixTxt BSearch = "?"

exCmdExecute :: [Text] -> HumState -> EventM Name (Next HumState)
exCmdExecute ("help":_) s = continue s { hview = HelpView }
exCmdExecute ("q":_) s = halt s
exCmdExecute ("save":name) s =
  let name' = if null name then "unnamed" else unwords name

  in
  do
  _ <- liftIO $ MPD.withMPD $ MPD.save (fromString . T.unpack $ name')
  continue =<< rebuildPl s
exCmdExecute _ s = continue s
