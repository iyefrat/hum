{-#LANGUAGE RankNTypes#-}
-- |

module Hum.Views.Library where
import           Prelude                 hiding ( Down )
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
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Hum.Utils



drawLibraryLeft :: HState -> Widget Name
drawLibraryLeft st =
  let vsize = case join $ Map.lookup LibraryLeft $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent LibraryLeft $ hCenter
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
  let vsize = case join $ Map.lookup LibraryMid $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent LibraryMid $ hCenter
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
  let vsize = case join $ Map.lookup LibraryRight $ extentMap st of
        Just e  -> snd . extentSize $ e
        Nothing -> 20
  in  reportExtent LibraryRight $ hCenter
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
        LibraryLeft -> queueArtistAttr
        LibraryMid  -> queueAlbumAttr
        _           -> listAttr
      )
    $ column Nothing (Pad 1) Max
    $ txt val

librarySongRow :: HState -> MPD.Song -> Widget n
librarySongRow st song =
  let pathsInQueue =
        (MPD.sgFilePath <$>) . (fst <$>) . listElements . queue $ st
  in  withAttr
          (if MPD.sgFilePath song `elem` pathsInQueue
            then queueTitleBoldAttr
            else queueTitleAttr
          )
        $ column Nothing (Pad 1) Max
        $ txt (meta (MPD.toText . MPD.sgFilePath $ song) MPD.Title song)

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
        songs <-
          liftIO
            (songsOfArtist
              (snd <$> listSelectedElement (s ^. libraryL . artistsL))
            )
        songBulkAddtoQ play songs s
      FocAlbums -> do
        songs <-
          liftIO
            (songsOfAlbum
              (snd <$> listSelectedElement (s ^. libraryL . albumsL))
            )
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
            extentMap <- updateExtentMap
            rebuildLibArtists
              $  (s { extentMap })
              &  libraryL
              .  artistsL
              %~ (dir . listFindBy (stringySearch searchkey) . dir)
          FocAlbums -> do
            extentMap <- updateExtentMap
            rebuildLibAlbums
              $  (s { extentMap })
              &  libraryL
              .  albumsL
              %~ (dir . listFindBy (stringySearch searchkey) . dir)
          FocSongs -> do
            extentMap <- updateExtentMap
            pure
              $  s { extentMap }
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
    EvKey (KChar 'G') [] ->
      continue =<< libraryMove (\ls -> listMoveBy (length ls) ls) s
    EvKey (KChar 'g') [] -> continue =<< libraryMove (listMoveTo 0) s -- TODO change this to  'gg', somehow
    _                    -> continue s
  _ -> continue s

addToPl :: EventM Name HState
addToPl = error "not implemented"
