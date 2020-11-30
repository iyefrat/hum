-- |

module Hum.Queue where

import           Brick.Widgets.List
import           Hum.Types
import qualified Network.MPD                   as MPD
import qualified Data.Vector                   as V

deleteHighlighted :: MPD.MonadMPD m => SongList -> m ()
deleteHighlighted ls =
  let hls = listFilter snd ls
  in  for_ hls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)
        >> whenJust
             ((MPD.sgId . fst . snd) =<< listSelectedElement ls)
             MPD.deleteId

deleteAll :: MPD.MonadMPD m => SongList -> m ()
deleteAll ls = for_ ls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)

pasteClipboard :: MPD.MonadMPD m => SongList -> SongList -> m ()
pasteClipboard clip ls =
  let pos         = listSelected ls
      indexedClip = V.indexed $ MPD.sgFilePath . fst <$> listElements clip
  in  for_ indexedClip (\(n, song) -> MPD.addId song $ (+ (n + 1)) <$> pos)

getHighlighted :: SongList -> SongList
getHighlighted ls = hls where
  hls = listFilter
    (\(el :: (MPD.Song, Highlight)) ->
      snd el || Just True == ((el ==) <$> (snd <$> listSelectedElement ls))
    )
    ls

listPaste
  :: (Splittable t, Semigroup (t e))
  => GenericList n t e
  -> GenericList n t e
  -> GenericList n t e
listPaste p ls =
  let es         = listElements ls
      pos        = fromMaybe 0 (listSelected ls)
      (es1, es2) = Brick.Widgets.List.splitAt (pos + 1) es
  in  ls { listElements = es1 <> listElements p <> es2 }

-- | toggle selected items highlight status
listToggleHighlight :: SongList -> SongList
listToggleHighlight = listModify (second not)
