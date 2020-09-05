-- |

module Ham.Attributes where

import           Brick.AttrMap
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.List             ( listAttr
                                                , listSelectedAttr
                                                , listSelectedFocusedAttr
                                                )
import qualified Brick.Util                    as BU
import           Graphics.Vty                   ( defAttr )
import qualified Graphics.Vty                  as Vty
import           Ham.Types


hamAttrMap :: HState -> AttrMap
hamAttrMap = const $ attrMap
  defAttr
  [ (listSelectedAttr   , Vty.withStyle defAttr Vty.reverseVideo)
  , (listHighlightedAttr, BU.fg Vty.yellow)
  , (headerAttr         , Vty.withStyle defAttr Vty.underline)
  , (queueAlbumAttr     , BU.fg Vty.red)
  , (queueTrackAttr     , BU.fg Vty.magenta)
  , (queueTitleAttr     , BU.fg Vty.cyan)
  , (queueArtistAttr    , BU.fg Vty.green)
  , (queueTimeAttr      , BU.fg Vty.blue)
  ]

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


highlightOverQueueAttrs :: Widget n -> Widget n
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
