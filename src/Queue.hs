-- |

module Queue where

import           Brick.Widgets.List
import           Types
import qualified Network.MPD                   as MPD

deleteHighlighted :: MPD.MonadMPD m => SongList -> m ()
deleteHighlighted ls =
  let hls = listFilter snd ls
  in  for_ hls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)
        >> whenJust
             ((MPD.sgId . fst . snd) =<< listSelectedElement ls)
             MPD.deleteId
pasteClipboard :: MPD.MonadMPD m => SongList -> m ()
pasteClipboard clip = for_ clip (\s -> MPD.add (MPD.sgFilePath . fst $ s))

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
      (es1, es2) = Brick.Widgets.List.splitAt pos es
  in  ls { listElements = es1 <> (listElements p) <> es2 }

--   My additions
-- | toggle selected items highlight status
listToggleHighlight2 :: SongList -> SongList
listToggleHighlight2 = listModify (\(sg, hl) -> (sg, not hl))


{-
listFilter
  :: (Foldable t, Splittable t, Applicative t, Monoid (t e))
  => (e -> Bool)
  -> GenericList n t e
  -> GenericList n t e

listFilter f ls =
  foldr (\x xs -> if f x then listInsert 0 x xs else xs) (listClear ls) ls
-}
