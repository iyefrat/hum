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
  , (queueAlbumAttr         , BU.fg Vty.red)
  , (queueTrackAttr         , BU.fg Vty.magenta)
  , (queueTitleAttr         , BU.fg Vty.cyan)
  , (queueArtistAttr        , BU.fg Vty.green)
  , (queueTimeAttr          , BU.fg Vty.blue)
  , (queueDateAttr          , BU.fg Vty.yellow)
  , ( queueNowPlayingAttr
    , Vty.withStyle (Vty.withStyle defAttr Vty.bold) Vty.underline
    )
  , (queueTitleBoldAttr, Vty.withStyle defAttr Vty.bold)
  ]

wobAttr :: AttrName
wobAttr = "white on black"

queueAttr, queueAlbumAttr, queueTitleAttr, queueTrackAttr, queueArtistAttr, queueTimeAttr, queueDateAttr
  :: AttrName
queueAttr = "queue"
queueAlbumAttr = queueAttr <> "album"
queueTitleAttr = queueAttr <> "title"
queueTrackAttr = queueAttr <> "track"
queueArtistAttr = queueAttr <> "artist"
queueTimeAttr = queueAttr <> "time"
queueDateAttr = queueAttr <> "date"

headerAttr :: AttrName
headerAttr = "header"

listHighlightedAttr :: AttrName
listHighlightedAttr = listAttr <> "highlighted"
queueNowPlayingAttr :: AttrName
queueNowPlayingAttr = queueAttr <> "now playing"

queueTitleBoldAttr :: AttrName
queueTitleBoldAttr = queueTitleAttr <> "bold"

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
