{-# OPTIONS_GHC  -Wno-orphans #-}

-- | Module    : Hum.Orphans
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--

module Hum.Orphans where

import qualified Witherable as W
import Brick.Widgets.List
import Control.Lens

-- | There isn't one obvious implementation fo this, so it can't be upstreamed.
instance W.Filterable t => W.Filterable (GenericList n t) where
  catMaybes l = l & listElementsL %~ W.catMaybes

instance (Traversable t, W.Filterable t) => W.Witherable (GenericList n t) where
