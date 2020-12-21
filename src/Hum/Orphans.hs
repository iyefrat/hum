{-# OPTIONS_GHC  -Wno-orphans#-}

module Hum.Orphans where

import qualified Network.MPD as MPD
import qualified Data.Witherable.Class as W
import Brick.Widgets.List
import Control.Lens

--instance Ord MPD.PlaylistName where
--  compare (MPD.PlaylistName x) (MPD.PlaylistName y) = compare x y

instance W.Filterable t => W.Filterable (GenericList n t) where
  catMaybes l = l & listElementsL %~ W.catMaybes

instance (Traversable t, W.Filterable t) => W.Witherable (GenericList n t) where
