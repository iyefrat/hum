-- |

module Queue where

import           Brick.Widgets.List
import           Types
import qualified Network.MPD                   as MPD

deleteHighlighted :: MPD.MonadMPD m => SongList -> m ()

deleteHighlighted ls = for_
  hls
  (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)
  where hls = listFilter snd ls

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