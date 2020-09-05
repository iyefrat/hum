-- |

module Ham.Attributes where

import           Brick.AttrMap
import           Brick.Widgets.Core
import           Brick.Widgets.List             ( listAttr
                                                , listSelectedAttr
                                                , listSelectedFocusedAttr
                                                )
import qualified Brick.Util                    as BU

queueAttr, queueAlbumAttr, queueTitleAttr, queueTrackAttr, queueArtistAttr, queueTimeAttr
  :: AttrName
queueAttr = "queue"
queueAlbumAttr = queueAttr <> "album"
queueTitleAttr = queueAttr <> "title"
queueTrackAttr = queueAttr <> "track"
queueArtistAttr = queueAttr <> "artist"
queueTimeAttr = queueAttr <> "time"

headerAttr :: AttrName
headerAttr = "header"




listHighlightedAttr :: AttrName
listHighlightedAttr = listAttr <> "highlighted"

highlightOverQueueAttrs = updateAttrMap
  (mapAttrNames
    (   (listHighlightedAttr, )
    <$> [ queueAlbumAttr
        , queueTrackAttr
        , queueTitleAttr
        , queueArtistAttr
        , queueTimeAttr
        ]
    )
  )
