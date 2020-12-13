-- |

module Hum.Attributes where

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


humAttrMap :: AttrMap
humAttrMap = attrMap
  defAttr
  [ (listAttr               , Vty.withStyle (BU.fg Vty.white) Vty.defaultStyleMask)
  , (listSelectedAttr       , Vty.withStyle defAttr Vty.underline)
  , (listSelectedFocusedAttr, Vty.withStyle defAttr Vty.reverseVideo)
  , (listHighlightedAttr    , BU.fg Vty.yellow)
  , (headerAttr             , Vty.withStyle defAttr Vty.underline)
  , (albumAttr         , BU.fg Vty.red)
  , (trackAttr         , BU.fg Vty.magenta)
  , (titleAttr         , BU.fg Vty.cyan)
  , (artistAttr        , BU.fg Vty.green)
  , (timeAttr          , BU.fg Vty.blue)
  , (dateAttr          , BU.fg Vty.yellow)
  , ( queueNowPlayingAttr
    , Vty.withStyle (Vty.withStyle defAttr Vty.bold) Vty.underline
    )
  , (titleBoldAttr, Vty.withStyle defAttr Vty.bold)
  , (editorAttr          , BU.bg Vty.black)
  ]

wobAttr :: Vty.Attr
wobAttr = BU.fg Vty.white

queueAttr, albumAttr, titleAttr, trackAttr, artistAttr, timeAttr, dateAttr
  :: AttrName
queueAttr = "queue"
albumAttr = queueAttr <> "album"
titleAttr = queueAttr <> "title"
trackAttr = queueAttr <> "track"
artistAttr = queueAttr <> "artist"
timeAttr = queueAttr <> "time"
dateAttr = queueAttr <> "date"

headerAttr :: AttrName
headerAttr = "header"

listHighlightedAttr :: AttrName
listHighlightedAttr = listAttr <> "highlighted"
queueNowPlayingAttr :: AttrName
queueNowPlayingAttr = queueAttr <> "now playing"

titleBoldAttr :: AttrName
titleBoldAttr = titleAttr <> "bold"


editorAttr :: AttrName
editorAttr = "editor"

highlightOverAttrs :: Widget n -> Widget n -- HACK?
highlightOverAttrs = updateAttrMap
  (mapAttrNames
    (   (listHighlightedAttr, )
    <$> [ albumAttr
        , trackAttr
        , titleAttr
        , artistAttr
        , timeAttr
        ]
    )
  )
