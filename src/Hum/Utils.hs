-- |

module Hum.Utils where
import           Hum.Types
import           Hum.Rebuild
import           Brick.Types
import           Brick.Widgets.List
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
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

deleteHighlightedfromQ :: MPD.MonadMPD m => SongList -> m ()
deleteHighlightedfromQ ls =
  let (hls :: SongList) = W.filter snd ls
  in  for_ hls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)
        >> whenJust
             ((MPD.sgId . fst . snd) =<< listSelectedElement ls)
             MPD.deleteId

deleteBulkfromQ :: MPD.MonadMPD m => SongList -> m ()
deleteBulkfromQ ls = for_ ls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)

pasteSongstoQ :: MPD.MonadMPD m => SongList -> SongList -> m ()
pasteSongstoQ clip ls =
  let pos         = listSelected ls
      indexedClip = V.indexed $ MPD.sgFilePath . fst <$> listElements clip
  in  for_ indexedClip (\(n, song) -> MPD.addId song $ (+ (n + 1)) <$> pos)

getHighlighted
  :: (W.Filterable t, Traversable t)
  => GenericList n t (e, Highlight)
  -> GenericList n t (e, Highlight)
getHighlighted ls = ls & listHighlightSelected ? W.filter snd ? listUnhighlightAll

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

deleteHighlighted :: HState -> Lens' HState SongList -> HState
deleteHighlighted st lns =
  st & clipboardL . clSongsL .~ (st ^. lns & getHighlighted)
     & lns %~ listHighlightSelected ? ( W.filter (not . snd))

yankHighlighted :: HState -> Lens' HState SongList -> HState
yankHighlighted st lns =
  st & clipboardL . clSongsL .~ (st ^. lns & getHighlighted)

-- | toggle selected items highlight status
listToggleHighlight :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listToggleHighlight = listModify (second not)


-- | Highlight selcted item status
listHighlightSelected :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listHighlightSelected = listModify (second (const True))

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

deleteSelectedPl :: HState -> EventM n HState
deleteSelectedPl st = do
  let plName = st ^. playlistsL . plListL & listSelectedElement <&> snd
  _ <- liftIO . withMPD $ traverse MPD.rm plName
  rebuildPl st

duplicatePlaylist :: MPD.PlaylistName -> HState -> EventM n HState -- HACK
duplicatePlaylist pl st = do
  songs <- V.fromList . fromRight [] <$> (liftIO . withMPD $ MPD.listPlaylistInfo pl)
  plNames <- (MPD.toText <$>) . fromRight [] <$> (liftIO . withMPD $ MPD.listPlaylists)
  let newPlName = viaNonEmpty head $ filter (`notElem` plNames) (map ((\tx num -> tx <> "-copy" <> show num) (MPD.toText pl)) [1::Int ..])
  traverse_ (\pln -> songBulkAddtoPl pln songs st) (T.unpack <$> newPlName)
  rebuildPl st

pastePlaylist :: HState -> EventM n HState
pastePlaylist st = do
  let plName = fromMaybe "<error>" (st ^. clipboardL . clPlNameL)
  duplicatePlaylist plName st

songBulkAddtoQ :: Bool -> V.Vector MPD.Song -> HState -> EventM n HState
songBulkAddtoQ play songs s = do
  let songPaths = MPD.sgFilePath <$> songs
  traverse_
    (\sel -> liftIO
      (withMPD $ MPD.addId sel Nothing >>= if play
        then MPD.playId
        else const pass
      )
    )
    (V.take 1 songPaths)
  traverse_ (\sel -> liftIO (withMPD $ MPD.addId sel Nothing))
            (V.drop 1 songPaths)
  pure s

songBulkAddtoPl :: String -> V.Vector MPD.Song -> HState -> EventM n HState
songBulkAddtoPl pl songs s = do
  let songPaths = MPD.sgFilePath <$> songs
  traverse_
    (\sel -> liftIO
      (withMPD $ MPD.playlistAdd (fromString pl) sel
      )
    )
    songPaths
  rebuildPl s
