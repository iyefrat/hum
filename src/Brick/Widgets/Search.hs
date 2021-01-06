-- |

module Brick.Widgets.Search where

import qualified Data.Text as T
import Hum.Types
import Brick.Types
import Brick.Widgets.Core

searchW :: n -> Text -> Text -> Widget n
searchW name term contents =
  let splits = lines <$> T.splitOn term contents
      mkWidget :: Widget n -> [Text] ->  Text -> [[Text]] -> Widget n
      mkWidget wid pre term posts = case (nonEmpty pre,nonEmpty posts) of
        (Nothing,_) -> wid
        (Just pre',Nothing) -> wid
                               <=> vBox (txt <$> init pre')
                               <=> (txt (last pre') <+>  (txt term))
        (Just pre',Just post') -> wid
                               <=> vBox (txt <$> init pre')
                               <=> (txt (last pre') <+> (txt term) <+> txt (head post'))
  in emptyWidget
