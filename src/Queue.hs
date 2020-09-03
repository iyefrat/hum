-- |

module Queue where

import           Brick.Widgets.List
import           Brick.AttrMap
import           Brick.Widgets.Core
import           Lens.Micro                     ( (^.)
--                                                , (^?)
--                                                , (&)
--                                               , (.~)
--                                               , (%~)
--                                               , _2
--                                               , _head
--                                               , set
                                                )

import           Brick.Types
import           Types
import qualified Network.MPD                   as MPD
import qualified Data.Vector                   as V

deleteHighlighted :: MPD.MonadMPD m => SongList -> m ()

deleteHighlighted ls = for_
  hls
  (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)
  where hls = listFilter snd ls

-- | toggle selected items highlight status
listToggleHighlight2 :: SongList -> SongList
listToggleHighlight2 = listModify (\(sg, hl) -> (sg, not hl))
