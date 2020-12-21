-- |

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



drawLibraryLeft :: HState -> Widget Name
drawLibraryLeft st =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ reportExtent LibraryLeft $ hCenter
        (   viewport LibraryLeft Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ libraryRow st LibraryLeft)
                          ((focLib . focus $ st) == FocArtists)
                          (MPD.toText <$> st ^. libraryL . artistsL)
              )
        )
drawLibraryMid :: HState -> Widget Name
drawLibraryMid st =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ reportExtent LibraryMid $ hCenter
        (   viewport LibraryMid Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ libraryRow st LibraryMid)
                          ((focLib . focus $ st) == FocAlbums)
                          (MPD.toText <$> st ^. libraryL . albumsL)
              )
        )


drawLibraryRight :: HState -> Widget Name
drawLibraryRight st =
  Widget Greedy Greedy $ do
    ctx <- getContext
    let vsize = ctx ^. windowHeightL - 6 -- HACK Don't hardcode nowplaying size?
    render $ reportExtent LibraryRight $ hCenter
        (   viewport LibraryRight Vertical
        .   visible
        .   vLimit vsize
        .   center
        $   hBorder
        <=> hCenter
              (renderList (const $ librarySongRow st)
                          ((focLib . focus $ st) == FocSongs)
                          (st ^. libraryL . songsL)
              )
        )


libraryRow :: HState -> Name -> T.Text -> Widget n
libraryRow _ name val =
  withAttr
      (case name of
        LibraryLeft -> artistAttr
        LibraryMid  -> albumAttr
        _           -> listAttr
      )
    $ column Nothing (Pad 1) Max
    $ txt val

libraryAlbumRow :: HState -> MPD.Song -> Widget n
libraryAlbumRow st song =
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


librarySongRow :: HState -> MPD.Song -> Widget n
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

libraryMoveRight :: FocLib -> FocLib
libraryMoveRight FocArtists = FocAlbums
libraryMoveRight _          = FocSongs

libraryMoveLeft :: FocLib -> FocLib
libraryMoveLeft FocSongs = FocAlbums
libraryMoveLeft _        = FocArtists

drawViewLibrary :: HState -> Widget Name
drawViewLibrary st =
  drawLibraryLeft st <+> drawLibraryMid st <+> drawLibraryRight st

libraryMove
  :: (forall e . List Name e -> List Name e) -> HState -> EventM Name HState
libraryMove moveFunc s =
  let libfoc = s ^. focusL . focLibL
  in  case libfoc of
        FocArtists -> rebuildLibArtists $ s & libraryL . artistsL %~ moveFunc
        FocAlbums  -> rebuildLibAlbums $ s & libraryL . albumsL %~ moveFunc
        FocSongs   -> do
          pure $ s & libraryL . songsL %~ moveFunc


libraryAddtoQ :: Bool -> HState -> EventM Name HState
libraryAddtoQ play s =
  let libfoc = s ^. focusL . focLibL
  in
    case libfoc of
      FocArtists -> do
        let martist = snd <$> listSelectedElement (s ^. libraryL . artistsL)
        songs <-liftIO $ maybe (pure empty) songsOfArtist martist
        songBulkAddtoQ play songs s
      FocAlbums -> do
        let malbum = snd <$> listSelectedElement (s ^. libraryL . albumsL)
        songs <- liftIO $ maybe (pure empty) songsOfAlbum malbum
        songBulkAddtoQ play songs s
      FocSongs -> do
        let songs = maybe V.empty V.singleton $ snd <$> listSelectedElement
              (s ^. libraryL . songsL)
        songBulkAddtoQ play songs s

librarySearch :: Bool -> HState -> EventM Name HState
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
              .  albumsL
              %~ (dir . listFindBy (stringySearch searchkey) . dir)
          FocSongs -> do
            pure
              $  s
              &  libraryL
              .  songsL
              %~ (dir . listFindBy (songSearch searchkey [MPD.Title]) . dir)


handleEventLibrary
  :: HState -> BrickEvent Name HumEvent -> EventM Name (Next HState)
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
    _                    -> continue s
  _ -> continue s
