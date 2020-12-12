-- |

module Hum.Utils where
import           Hum.Types
import           Brick.Types
import           Brick.Main
import           Brick.Widgets.List
import           Data.Vector                   as V
import           Data.Text                     as T
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Text.Printf                    ( printf )
import           Control.Lens
import qualified Data.Witherable.Class         as W

-- | A backwards function composition operator
infixl 8  ?
{-# INLINE (?) #-}
-- Make sure it has TWO args only on the left, so that it inlines
-- when applied to two functions, even if there is no final argument
(?)    :: (a -> b) -> (b -> c) -> a -> c
(?) f g = \x -> g (f x)

-- | Get comma seperated metedata from tag
meta :: Text -> MPD.Metadata -> MPD.Song -> Text
meta notFound tag song = maybe
  notFound
  (T.intercalate ",")
  (MPD.toText <<$>> Map.lookup tag (MPD.sgTags song))

-- | like meta, but returns a Maybe for future use
mmeta :: MPD.Metadata -> MPD.Song -> Maybe Text
mmeta tag song =
  T.intercalate "," <$> (MPD.toText <<$>> Map.lookup tag (MPD.sgTags song))


secondsToTime :: Integer -> Text
secondsToTime sec =
  let (minutes, seconds) = divMod sec 60
  in  toText (printf "%d:%02d" minutes seconds :: String)


songsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfArtist martist = V.fromList . fromRight [] <$> withMPD
  (MPD.find (MPD.AlbumArtist MPD.=? fromMaybe "" martist))

songsOfAlbum :: Maybe MPD.Value -> IO (V.Vector MPD.Song)
songsOfAlbum malbum = V.fromList . fromRight [] <$> withMPD
  (MPD.find (MPD.Album MPD.=? fromMaybe "" malbum))
albumsOfArtist :: Maybe MPD.Value -> IO (V.Vector MPD.Value)
albumsOfArtist martist =
  V.fromList . fromRight [] <$> withMPD (MPD.list MPD.Album martist)

updateExtentMap :: EventM Name (Map Name (Maybe (Extent Name)))
updateExtentMap = do
  queueE      <- lookupExtent Queue
  nowPlayingE <- lookupExtent NowPlaying
  libLeftE    <- lookupExtent LibraryLeft
  libMidE     <- lookupExtent LibraryMid
  libRightE   <- lookupExtent LibraryRight
  playLeftE   <- lookupExtent PlaylistLeft
  playRightE  <- lookupExtent PlaylistRight
  let extentMap = Map.fromList
        [ (Queue        , queueE)
        , (NowPlaying   , nowPlayingE)
        , (LibraryLeft  , libLeftE)
        , (LibraryMid   , libMidE)
        , (LibraryRight , libRightE)
        , (PlaylistLeft , playLeftE)
        , (PlaylistRight, playRightE)
        ]
  pure extentMap

deleteHighlightedfromQ :: MPD.MonadMPD m => SongList -> m ()
deleteHighlightedfromQ ls =
  let (hls :: SongList) = W.filter snd ls
  in  for_ hls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)
        >> whenJust
             ((MPD.sgId . fst . snd) =<< listSelectedElement ls)
             MPD.deleteId

deleteAll :: MPD.MonadMPD m => SongList -> m ()
deleteAll ls = for_ ls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)

pasteClipboardtoQ :: MPD.MonadMPD m => SongList -> SongList -> m ()
pasteClipboardtoQ clip ls =
  let pos         = listSelected ls
      indexedClip = V.indexed $ MPD.sgFilePath . fst <$> listElements clip
  in  for_ indexedClip (\(n, song) -> MPD.addId song $ (+ (n + 1)) <$> pos)

getHighlighted
  :: (Eq e, W.Filterable t, Foldable t, Splittable t)
  => GenericList n t (e, Highlight)
  -> GenericList n t (e, Highlight)
getHighlighted ls = hls where
  hls = W.filter
    (\(el, hl) -> hl || Just (el, hl) == (snd <$> listSelectedElement ls))
    ls

listPaste
  :: (Splittable t, Semigroup (t e))
  => GenericList n t e
  -> GenericList n t e
  -> GenericList n t e
listPaste paste ls =
  let es         = listElements ls
      pos        = fromMaybe 0 (listSelected ls)
      (es1, es2) = Brick.Widgets.List.splitAt (pos + 1) es
  in  ls { listElements = es1 <> listElements paste <> es2 }

deleteHighlighted ::  HState
    -> Lens' HState SongList
    -> HState
deleteHighlighted st lns = st & clipboardL .~ (st ^. lns & listHighlight ? W.filter snd ? listUnhighlightAll)
                              & lns %~ listHighlight ? W.filter (not . snd)

-- | toggle selected items highlight status
listToggleHighlight :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listToggleHighlight = listModify (second not)


-- | Highlight selcted item status
listHighlight :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listHighlight = listModify (second (const True))

listUnhighlightAll :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listUnhighlightAll = fmap (second $ const False)

saveListToPl :: MPD.MonadMPD m => SongList -> Text -> m ()
saveListToPl ls name =
  let songpaths = MPD.sgFilePath . fst <$> listElements ls
      name'     = fromString . T.unpack $ name
  in  for_ songpaths (MPD.playlistAdd name')

overwriteListToPl :: MPD.MonadMPD m => SongList -> Text -> m ()
overwriteListToPl ls name =
  let songpaths = MPD.sgFilePath . fst <$> listElements ls
      name'     = fromString . T.unpack $ name
  in MPD.playlistClear name' >>
     for_ songpaths (MPD.playlistAdd name')

saveEditedPl :: HState -> EventM n HState
saveEditedPl st = do
  let plSongs = st ^. playlistsL . plSongsL
  let plName = st ^. playlistsL . plListL & listSelectedElement ? maybe "unnamed" snd ? MPD.toText
  _ <- liftIO . withMPD $ overwriteListToPl plSongs plName
  pure st
