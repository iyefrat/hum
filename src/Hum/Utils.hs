
-- | Module    : Hum.Utils
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--


module Hum.Utils where
import           Hum.Types
import           Hum.Rebuild
import           Brick.Types
import           Brick.Widgets.List
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import qualified Data.ByteString               as BS
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Text.Printf                    ( printf )
import           Control.Lens
import qualified Data.Witherable.Class         as W

-- | A backwards function composition operator that I love with my whole heart.
infixl 8  ?
{-# INLINE (?) #-}
-- Make sure it has TWO args only on the left, so that it inlines
-- when applied to two functions, even if there is no final argument
(?)    :: (a -> b) -> (b -> c) -> a -> c
(?) f g = \x -> g (f x)

-- | Get comma seperated metedata from tag.
meta :: Text -> MPD.Metadata -> MPD.Song -> Text
meta notFound tag song = maybe
  notFound
  (T.intercalate ",")
  (MPD.toText <<$>> Map.lookup tag (MPD.sgTags song))

-- | Like 'meta', but returns a Maybe for future use.
mmeta :: MPD.Metadata -> MPD.Song -> Maybe Text
mmeta tag song =
  T.intercalate "," <$> (MPD.toText <<$>> Map.lookup tag (MPD.sgTags song))

-- | Formats seconds to %M:%S.
secondsToTime :: Integer -> Text
secondsToTime sec =
  let (minutes, seconds) = divMod sec 60
  in  toText (printf "%d:%02d" minutes seconds :: String)

-- | Deletes highlighted songs in list from queue, does not rebuild queue.
deleteHighlightedfromQ :: MPD.MonadMPD m => SongList -> m ()
deleteHighlightedfromQ ls =
  let (hls :: SongList) = W.filter snd ls
  in  for_ hls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)
        >> whenJust
             ((MPD.sgId . fst . snd) =<< listSelectedElement ls)
             MPD.deleteId

-- | Deletes list of songs from queue in MPD, does not rebuild queue.
deleteBulkfromQ :: MPD.MonadMPD m => SongList -> m ()
deleteBulkfromQ ls = for_ ls (\s -> whenJust (MPD.sgId . fst $ s) MPD.deleteId)

-- | Adds songs to queue under the selected item in it in MPD, does not rebuild queue.
pasteSongstoQ :: MPD.MonadMPD m => SongList -> SongList -> m () -- TODO refactor to act on HumState
pasteSongstoQ clip ls =
  let pos         = listSelected ls
      indexedClip = V.indexed $ MPD.sgFilePath . fst <$> listElements clip
  in  for_ indexedClip (\(n, song) -> MPD.addId song $ (+ (n + 1)) <$> pos)

-- | Produce list of highligted elements (and selected element) in input list.
getHighlighted
  :: (W.Filterable t, Traversable t)
  => GenericList n t (e, Highlight)
  -> GenericList n t (e, Highlight)
getHighlighted ls = ls & listHighlightSelected ? W.filter snd ? listUnhighlightAll

-- | Paste one list into another under the selected item.
listPaste
  :: (Splittable t, Semigroup (t e))
  => GenericList n t e -- ^ List pasted into
  -> GenericList n t e -- ^ Pasted list
  -> GenericList n t e
listPaste paste ls =
  let es         = listElements ls
      pos        = fromMaybe 0 (listSelected ls)
      (es1, es2) = Brick.Widgets.List.splitAt (pos + 1) es
  in  ls { listElements = es1 <> listElements paste <> es2 }

-- | Delete highlighted element (and selected element) from list.
deleteHighlighted
  :: HumState
  -> Lens' HumState SongList -- ^ Lens that leads to list
  -> HumState
deleteHighlighted st lns =
  st & clipboardL . clSongsL .~ (st ^. lns & getHighlighted)
     & lns %~ listHighlightSelected ?  W.filter (not . snd)

-- | Copy highlighted element (and selected element) from list to 'Clipboard'.
yankHighlighted
  :: HumState
  -> Lens' HumState SongList -- ^ Lens that leads to list
  -> HumState
yankHighlighted st lns =
  st & clipboardL . clSongsL .~ (st ^. lns & getHighlighted)

-- | Toggle selected items highlight status.
listToggleHighlight :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listToggleHighlight = listModify (second not)


-- | Highlight selcted item.
listHighlightSelected :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listHighlightSelected = listModify (second (const True))

-- | Unhighlight selcted item.
listUnhighlightAll :: Traversable t => GenericList n t (e,Highlight) -> GenericList n t (e,Highlight)
listUnhighlightAll = fmap (second $ const False)

-- | Save list of songs to a stored playlist. If exists does nothing.
saveListToPl :: MPD.MonadMPD m =>
     SongList
  -> Text -- ^ Name of playlist to save to
  -> m () -- TODO use unusedPlName
saveListToPl ls name =
  let songpaths = MPD.sgFilePath . fst <$> listElements ls
      name'     = fromString . T.unpack $ name
  in  for_ songpaths (MPD.playlistAdd name')

-- | Overwrite stored playlist with new song list.
overwriteListToPl :: MPD.MonadMPD m => SongList -> Text -> m ()
overwriteListToPl ls name =
  let songpaths = MPD.sgFilePath . fst <$> listElements ls
      name'     = fromString . T.unpack $ name
  in MPD.playlistClear name' >>
     for_ songpaths (MPD.playlistAdd name')

-- | Save edited playlist in Playlist view to disk.
saveEditedPl :: HumState -> EventM n HumState
saveEditedPl st = do
  let plSongs = st ^. playlistsL . plSongsL
  let plName = st ^. playlistsL . plListL & listSelectedElement ? maybe "unnamed" snd ? MPD.toText
  _ <- liftIO . withMPD $ overwriteListToPl plSongs plName
  pure st

-- | Deletes selected playlist in Playlist view from disk.
deleteSelectedPl :: HumState -> EventM n HumState
deleteSelectedPl st = do
  let plName = st ^. playlistsL . plListL & listSelectedElement <&> snd
  _ <- liftIO . withMPD $ traverse MPD.rm plName
  rebuildPl st

-- | Appends smallest number possible to playlist name for it to not be taken.
-- Does nothing if name is untaken.
unusedPlName :: MPD.PlaylistName -> IO MPD.PlaylistName
unusedPlName prefix = do
  plNames <- fromRight [] <$> (liftIO . withMPD $ MPD.listPlaylists)
  let newPlName = viaNonEmpty head $ filter (`notElem` plNames) (prefix:(append' prefix . show <$> [2::Int ..]))
  pure (fromMaybe "unnamed" newPlName) -- HACK
  where
    append' (MPD.PlaylistName x) (MPD.PlaylistName y) = MPD.PlaylistName (BS.append x y)

-- | Duplicates stored playlist on disk (with nonconflicting name).
duplicatePlaylist :: MPD.PlaylistName -> HumState -> EventM n HumState -- HACK
duplicatePlaylist pl st = do
  songs <- V.fromList . fromRight [] <$> (liftIO . withMPD $ MPD.listPlaylistInfo pl)
  newPlName <- liftIO $ unusedPlName pl
  _ <- songBulkAddtoPl (MPD.toString newPlName) songs st
  rebuildPl st

-- | Pastes playlist in clipboard to disk (with nonconflicting name).
pastePlaylist :: HumState -> EventM n HumState
pastePlaylist st = do
  let plName = fromMaybe "<error>" (st ^. clipboardL . clPlNameL)
  duplicatePlaylist plName st

-- | Adds list of songs to queue in MPD. Does not rebuild state.
songBulkAddtoQ
  :: Bool -- ^ If true plays first song added
  -> V.Vector MPD.Song
  -> HumState
  -> EventM n HumState
songBulkAddtoQ play songs s = do -- TODO don't need s? maybe should rebuild? overlap with system in Hum.UI
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

-- | Adds list of songs to stored playlist in MPD. Does not rebuild state.
songBulkAddtoPl
  :: String -- ^ Playlist Name
  -> V.Vector MPD.Song -- ^ Songs to add
  -> HumState
  -> EventM n HumState
songBulkAddtoPl pl songs s = do
  let songPaths = MPD.sgFilePath <$> songs
  traverse_
    (\sel -> liftIO
      (withMPD $ MPD.playlistAdd (fromString pl) sel
      )
    )
    songPaths
  rebuildPl s
