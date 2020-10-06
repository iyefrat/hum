-- |

module Ham.Modes.SearchMode where

import           Brick.Widgets.Edit      hiding ( decodeUtf8 )
import qualified Brick.Widgets.Edit            as E
                                                ( decodeUtf8 )
import           Brick.Types
import           Brick.Main
import           Ham.Types
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
  :: HState -> BrickEvent Name HamEvent -> EventM Name (Next HState)
handleSearchEvent s e = case e of
  VtyEvent (EvKey KEnter []) -> continue $ s & modeL .~ NormalMode
  VtyEvent vtye ->
    continue =<< handleEventLensed s (searchL) handleSearchEditorEvent vtye
  _ -> continue s

handleSearchEditorEvent
  :: (DecodeUtf8 t, Eq t, Monoid t)
  => Event
  -> Editor t n
  -> EventM n (Editor t n)
handleSearchEditorEvent e ed =
  let f = case e of
        EvPaste bs -> case E.decodeUtf8 bs of
          Left  _ -> id
          Right t -> Z.insertMany t
        EvKey (KChar 'a') [MCtrl]      -> Z.gotoBOL
        EvKey (KChar 'e') [MCtrl]      -> Z.gotoEOL
        EvKey (KChar 'd') [MCtrl]      -> Z.deleteChar
        EvKey (KChar 'k') [MCtrl]      -> Z.killToEOL
        EvKey (KChar 'u') [MCtrl]      -> Z.killToBOL
        EvKey KEnter      []           -> Z.breakLine
        EvKey KDel        []           -> Z.deleteChar
        EvKey (KChar c) [] | c /= '\t' -> Z.insertChar c
        EvKey KUp    []                -> Z.moveUp
        EvKey KDown  []                -> Z.moveDown
        EvKey KLeft  []                -> Z.moveLeft
        EvKey KRight []                -> Z.moveRight
        EvKey KBS    []                -> Z.deletePrevChar
        EvKey KHome  []                -> Z.gotoBOL
        EvKey KEnd   []                -> Z.gotoEOL
        _                              -> id
  in  pure $ applyEdit f ed
