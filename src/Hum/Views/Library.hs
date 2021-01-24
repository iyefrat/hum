
-- | Module    : Hum.Views.Library
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- Functions for the Library view.

module Hum.Views.Library where
import           Hum.Types
import           Brick.Types
import           Graphics.Vty.Input.Events
import           Brick.Main
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Control.Lens
import           Brick.Widgets.List
import           Hum.Attributes
import           Hum.Views.Common
import           Hum.Rebuild
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Network.MPD                   as MPD
import           Hum.Utils

-- | Draw left column in Library view.
drawLibraryLeft :: HumState -> Widget Name
drawLibraryLeft st = reportExtent LibraryLeft $
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ hCenter
        (   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ libraryRow st LibraryLeft)
                          ((focLib . focus $ st) == FocArtists)
                          (MPD.toText <$> st ^. libraryL . artistsL)
              )
        )

-- | Draw middle column in Library view.
drawLibraryMid :: HumState -> Widget Name
drawLibraryMid st = reportExtent LibraryMid $
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ hCenter
        (   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ libraryAlbumRow)
                          ((focLib . focus $ st) == FocAlbums)
                          (st ^. libraryL . yalbumsL)
              )
        )

-- | Draw right column in Library view.
drawLibraryRight :: HumState -> Widget Name
drawLibraryRight st = reportExtent LibraryRight $
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ hCenter
        (   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ librarySongRow st)
                          ((focLib . focus $ st) == FocSongs)
                          (st ^. libraryL . songsL)
              )
        )

-- | Draw generic column row in Library view.
libraryRow :: HumState -> Name -> T.Text -> Widget n -- TODO refactor?
libraryRow _ name val =
  withAttr
      (case name of
        LibraryLeft -> artistAttr
        LibraryMid  -> albumAttr
        _           -> listAttr
      )
    $ column Nothing (Pad 1) Max
    $ txt val

-- | Draw row in album column in Library view.
libraryAlbumRow :: (MPD.Value,MPD.Value) -> Widget n
libraryAlbumRow (yr,al) =
  let year = MPD.toText yr
      album = MPD.toText al
      yearW = withAttr dateAttr $ column (Just (Col 7)) Max Max $
        if T.null year
          then txt "      "
         else  txt "(" <+> txt (T.take 4 year) <+> txt ")"
      albumW = withAttr albumAttr $ column Nothing Max Max $ txt album
  in yearW <+> albumW

-- | Draw row in song column in Library view.
librarySongRow :: HumState -> MPD.Song -> Widget n
librarySongRow st song =
  let pathsInQueue =
        (MPD.sgFilePath <$>) . (fst <$>) . listElements . queue $ st
      title  = meta (MPD.toText . MPD.sgFilePath $ song) MPD.Title song
      titleW =  withAttr titleAttr $ column Nothing Max Max $ txt title
      track = meta "-" MPD.Track song
      trackW = withAttr trackAttr $ column (Just (Col 3)) Max (Pad 1) $ txt track
  in     (if MPD.sgFilePath song `elem` pathsInQueue
            then withAttr titleBoldAttr
            else id
          )
        $ trackW <+> titleW

-- | Move focus right in Library view.
libraryMoveRight :: FocLib -> FocLib
libraryMoveRight FocArtists = FocAlbums
libraryMoveRight _          = FocSongs

-- | Move focus left in Library view.
libraryMoveLeft :: FocLib -> FocLib
libraryMoveLeft FocSongs = FocAlbums
libraryMoveLeft _        = FocArtists

-- | Draw Library view.
drawViewLibrary :: HumState -> Widget Name
drawViewLibrary st =
  drawLibraryLeft st <+> drawLibraryMid st <+> drawLibraryRight st

-- | Move focused library column by given function
libraryMove
  :: (forall e . List Name e -> List Name e) -- ^ Function to move the focused column with
  -> HumState
  -> EventM Name HumState
libraryMove moveFunc s =
  let libfoc = s ^. focusL . focLibL
  in  case libfoc of
        FocArtists -> rebuildLibArtists $ s & libraryL . artistsL %~ moveFunc
        FocAlbums  -> rebuildLibAlbums $ s & libraryL . yalbumsL %~ moveFunc
        FocSongs   -> do
          pure $ s & libraryL . songsL %~ moveFunc

-- | Add selected element in Library view to queue.
-- If the element is an album or artist add all songs under it.
libraryAddtoQ
  :: Bool -- ^ Play first item added to queue
  -> HumState
  -> EventM Name HumState
libraryAddtoQ play s =
  let libfoc = s ^. focusL . focLibL
  in
    case libfoc of
      FocArtists -> do
        let martist = snd <$> listSelectedElement (s ^. libraryL . artistsL)
        songs <-liftIO $ maybe (pure empty) songsOfArtist martist
        songBulkAddtoQ play songs s
      FocAlbums -> do
        let malbum = snd . snd <$> listSelectedElement (s ^. libraryL . yalbumsL)
        songs <- liftIO $ maybe (pure empty) songsOfAlbum malbum
        songBulkAddtoQ play songs s
      FocSongs -> do
        let songs = maybe V.empty V.singleton $ snd <$> listSelectedElement
              (s ^. libraryL . songsL)
        songBulkAddtoQ play songs s

-- | Search focused library column for next instance of last search.
librarySearch
  :: Bool -- ^ Search direction, True for forward.
  -> HumState
  -> EventM Name HumState
librarySearch direction s =
  let libfoc    = s ^. focusL . focLibL
      dir       = if direction then id else listReverse
      searchkey = fromMaybe "" ((s ^. exL . searchHistoryL) !!? 0)
  in  if searchkey == ""
        then pure s
        else case libfoc of
          FocArtists -> do
            rebuildLibArtists
              $  s
              &  libraryL
              .  artistsL
              %~ (dir . listFindBy (stringySearch searchkey) . dir)
          FocAlbums -> do
            rebuildLibAlbums
              $  s
              &  libraryL
              .  yalbumsL
              %~ (dir . listFindBy (stringySearch searchkey . snd) . dir)
          FocSongs -> do
            pure
              $  s
              &  libraryL
              .  songsL
              %~ (dir . listFindBy (songSearch searchkey [MPD.Title]) . dir)

-- | handle key inputs for Library view.
handleEventLibrary
  :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleEventLibrary s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'j') [] -> continue =<< libraryMove listMoveDown s
    EvKey (KChar 'k') [] -> continue =<< libraryMove listMoveUp s
    EvKey (KChar 'l') [] -> continue $ s & focusL . focLibL %~ libraryMoveRight
    EvKey (KChar 'h') [] -> continue $ s & focusL . focLibL %~ libraryMoveLeft
    EvKey (KChar 'n') [] ->
      continue =<< librarySearch (s ^. exL . searchDirectionL) s
    EvKey (KChar 'N') [] ->
      continue =<< librarySearch (s ^. exL . searchDirectionL & not) s
    EvKey KEnter [] ->
      continue =<< libraryMove listMoveDown =<< libraryAddtoQ True s
    EvKey (KChar ' ') [] ->
      continue =<< libraryMove listMoveDown =<< libraryAddtoQ False s
    EvKey (KChar 'G') [] -> continue =<< libraryMove (listMoveTo (-1)) s
    EvKey (KChar 'g') [] -> continue =<< libraryMove (listMoveTo 0) s -- TODO change this to  'gg', somehow
    EvKey (KChar '`') [] -> continue =<< rebuildLibArtists (s & libraryL . yalbumSortL %~ not)
    _                    -> continue s
  _ -> continue s
