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
  let (hls :: SongList) = listFilter snd ls
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

getHighlighted :: SongList -> SongList
getHighlighted ls = hls where
  hls = listFilter
    (\(song,hl) ->
      hl || Just (song,hl) == (snd <$> listSelectedElement ls)
    )
    ls

listPaste
  :: (Splittable t, Semigroup (t e))
  => GenericList n t e
  -> GenericList n t e
  -> GenericList n t e
listPaste p ls =
  let es         = listElements ls
      pos        = fromMaybe 0 (listSelected ls)
      (es1, es2) = Brick.Widgets.List.splitAt (pos + 1) es
  in  ls { listElements = es1 <> listElements p <> es2 }

-- | toggle selected items highlight status
listToggleHighlight :: SongList -> SongList
listToggleHighlight = listModify (second not)

saveListToPl :: MPD.MonadMPD m => SongList -> Text -> m ()
saveListToPl ls name =
  let songpaths = MPD.sgFilePath . fst <$> listElements ls
      name'     = fromString . T.unpack $ name
  in  for_ songpaths (MPD.playlistAdd name')

