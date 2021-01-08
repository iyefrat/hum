-- |

module Brick.Widgets.Search where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import qualified Data.Text as T
import Hum.Types
import Hum.Attributes
import Brick.Types
import Brick.Widgets.Core
import qualified Data.Array as A

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
          <+> ((if hl then
                  visible . withAttr searchFocusedAttr
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

splitRegex :: Regex -> Text -> [[(Text, Text)]]
splitRegex rg source = go [] rg <$> lines source
 where
  go :: [(Text, Text)] -> Regex -> Text -> [(Text, Text)]
  go prev rg rest =
    let match = matchOnceText rg rest
    in  case match of
          Nothing              -> prev ++ [(rest, "")]
          Just (pre, mt, post) -> go
            (prev ++ [(pre, maybe "" fst $ viaNonEmpty head (A.elems mt))])
            rg
            post

regexW :: Int -> Regex -> Text -> Widget n
regexW highlight term contents =
  let splits = splitRegex term contents
      numMatches = sum $ (\ls -> max 0 (length ls - 1)) <$> splits
      mkLine :: [(Text,Text)] -> State Int (Widget n)
      mkLine [] = pure emptyWidget
      mkLine [("",_)] = pure $ txt " "
      mkLine [(tx,_)] = pure $ txt tx
      mkLine ((tx,mtch):ts) = do
        num <- get
        let hl = (num - highlight) `mod` numMatches == 0
        modify (+1)
        let (rest,numFinal) = runState (mkLine ts) (num+1)
        put numFinal
        pure $ txt tx
          <+> ((if hl then
                  visible . withAttr searchFocusedAttr
                else withAttr searchAttr) $ txt mtch)
          <+> rest
      mkWidget :: [[(Text,Text)]] -> State Int (Widget n)
      mkWidget [] = pure emptyWidget
      mkWidget [t] = join . pure $ mkLine t
      mkWidget (t:ts) = do
        num <- get
        let (line,numLine) = runState (mkLine t) num
        let (rest,numFinal) = runState (mkWidget ts) numLine
        put numFinal
        pure $ line <=> rest
      in evalState (mkWidget splits) 0
