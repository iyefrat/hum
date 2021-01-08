-- |

module Brick.Widgets.Search where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import qualified Data.Text as T
import Hum.Types
import Hum.Attributes
import Brick.Types
import Brick.Widgets.Core

searchW :: Int -> Text -> Text -> Widget n
searchW highlight term contents =
  let splits = T.splitOn term <$> lines contents
      numMatches = sum $ (\ls -> max 0 (length ls - 1)) <$> splits
      mkLine :: [Text] -> State Int (Widget n)
      mkLine [] = pure emptyWidget
      mkLine [""] = pure $ txt " "
      mkLine [t] = pure $ txt t
      mkLine (t:ts) = do
        num <- get
        let hl = (num - highlight) `mod` numMatches == 0
        modify (+1)
        let (rest,numFinal) = runState (mkLine ts) (num+1)
        put numFinal
        pure $ txt t
          <+> ((if hl
                then visible . withAttr searchFocusedAttr
                else withAttr searchAttr) $ txt term)
          <+> rest
      mkWidget :: [[Text]] -> State Int (Widget n)
      mkWidget [] = pure emptyWidget
      mkWidget [t] = join . pure $ mkLine t
      mkWidget (t:ts) = do
        num <- get
        let (line,numLine) = runState (mkLine t) num
        let (rest,numFinal) = runState (mkWidget ts) numLine
        put numFinal
        pure $ line <=> rest
      in evalState (mkWidget splits) 0

{-
splitRegex :: Regex -> Text -> [[Text]]
splitRegex rg source =
  let matches :: Regex -> Text -> Text
      matches rg =

  in []
-- regexW :: Int -> Regex -> Text -> Widget n
-}
