
-- | Module    : Hum.UI
-- Copyright   : (c) Itai Y. Efrat 2020-2021
-- License     : GPLv2-or-later (see LICENSE)
-- Maintainer  : Itai Y. Efrat <itai3397@gmail.com>
--
-- The starting point of Hum's logic. Contains things like building the initial
-- state, shared keypresses, and the UI drawing function.

module Hum.UI where


import           Brick.Main
import           Brick.Types
import qualified Brick.BChan                   as BC
import           Brick.Widgets.Core
import           Brick.Widgets.List
import           Brick.Widgets.Edit
import           Graphics.Vty.Input.Events
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Hum.Types
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Hum.Attributes
import           Hum.Views
import           Hum.Modes
import           Hum.Rebuild
import           Control.Lens
import           System.Directory

-- | The brick app record for Hum.
app :: App HumState HumEvent Name
app = App { appDraw         = drawUI
          , appChooseCursor = chooseCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = humStartEvent
          , appAttrMap      = const humAttrMap
          }

-- | Draws shared UI elements and dispatches for view specific ones.
drawUI :: HumState -> [Widget Name]
drawUI st =
  [if st ^. modeL == PromptMode then drawPrompt st else emptyWidget,
   drawNowPlaying st
    <=> (case hview st of
          QueueView     -> drawViewQueue st
          LibraryView   -> drawViewLibrary st
          PlaylistsView -> drawViewPlaylists st
          HelpView      -> drawViewHelp st
        )
    <=> if st ^. focusL . focExL
          then txt (st ^. exL . exPrefixL & exPrefixTxt) <+> renderEditor
            (txt . T.unlines)
            (st ^. focusL . focExL)
            (st ^. exL . exEditorL)
          else txt " "
  ]

-- | Make sure cursor is displayed when editing text.
chooseCursor :: HumState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
chooseCursor st ls
  | st ^. focusL . focExL = find (isCurrent ExEditor) ls
  | st ^. promptsL . currentPromptL == TextPrompt = find (isCurrent TextPromptEditor) ls
  | otherwise = Nothing
  where
      isCurrent n cl = cl ^. cursorLocationNameL == Just n

-- | builds ''HumState' for the first time. A lot of overlap with functions from
-- | "Hum.Rebuild", but they are hard to reuse because 'HumState' is strict.
buildInitialState :: BC.BChan HumEvent -> IO HumState
buildInitialState chan = do
  configDir <- getXdgDirectory XdgConfig "hum"
  _ <- createDirectoryIfMissing True configDir
  let mode = NormalMode
  let ex = ExState { exPrefix        = Cmd
                   , exEditor        = editorText ExEditor (Just 1) ""
                   , searchDirection = True
                   , searchHistory   = []
                   , cmdHistory      = []
                   }
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  let hview      = QueueView
  let focus = Focus { focQueue = FocQueue
                    , focLib   = FocArtists
                    , focPlay  = FocPlaylists
                    , focEx    = False
                    }
  let clipboard = Clipboard { clSongs = list ClSongs V.empty 1
                            , clPlName = Nothing}
  queueVec <- V.fromList . fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  let queue = (, False) <$> list QueueList queueVec 1
  artistsVec <- V.fromList . fromRight [] <$> withMPD (MPD.list MPD.AlbumArtist mempty)
  let artists = list ArtistsList artistsVec 1
  let yalbumSort = True
  yalbumsVec <- maybe (pure empty) (yalbumsOfArtist yalbumSort) (snd <$> listSelectedElement artists)
  let yalbums = list YalbumsList yalbumsVec 1
  songsVec <- maybe (pure empty) songsOfAlbum (snd . snd <$> listSelectedElement yalbums)
  let songs = list SongsList songsVec 1
  let library = LibraryState { artists, yalbums, yalbumSort, songs }
  plListVec <- V.fromList . sort . fromRight [] <$> withMPD MPD.listPlaylists
  let plList = list PlaylistList plListVec 1
  plSongsVec <- V.fromList . fromRight [] <$> withMPD
    ( MPD.listPlaylistInfo
    $ maybe "<no playlists>" snd (listSelectedElement plList)
    )
  let plSongs = (, False) <$> list PlaylistSongs plSongsVec 1
  let playlists = PlaylistsState { plList, plSongs }
  let editable  = False
  let prompts = Prompts
        { currentPrompt  = PlSelectPrompt
        , promptTitle    = ""
        , textPrompt     = editorText TextPromptEditor (Just 1) ""
        , plSelectPrompt = listInsert 0 Nothing (Just <$> plList)
        , exitPromptFunc = \_ s -> pure $ s & modeL .~ NormalMode
        }
  let help = HelpState
        { helpText = helpText'
        , helpSearchInt = 0
        }
  pure HumState { chan
              , hview
              , mode
              , ex
              , status
              , currentSong
              , queue
              , clipboard
              , library
              , playlists
              , help
              , focus
              , editable
              , prompts
              }

-- | Initial event.
humStartEvent :: HumState -> EventM Name HumState
humStartEvent = pure

-- | handles keypresses shared between views, and nonkeypress events from the
-- channels (ticks every 0.1 seconds and mpd events). Dispatches for view
-- specific keypresses.
handleEvent :: HumState -> BrickEvent Name HumEvent -> EventM Name (Next HumState)
handleEvent s e = case e of
  VtyEvent vtye -> case s ^. modeL of
    ExMode     -> handleExEvent s e
    PromptMode -> handlePromptEvent s e
    NormalMode -> case vtye of
      EvKey (KChar 'q') [] -> halt s
      EvKey (KChar 't') [] -> do
        st <- liftIO ((MPD.stState <$>) <$> withMPD MPD.status)
        _  <- case st of
          Left  _  -> liftIO (withMPD $ MPD.pause True)
          Right _  -> liftIO (withMPD $ MPD.toggle)
        continue s
      EvKey (KChar 's') [] -> do
        _ <- liftIO
          (withMPD $ MPD.single (maybe False (not . MPD.stSingle) (status s)))
        continue =<< rebuildStatus s
      EvKey (KChar 'c') [] -> do
        _ <-
          liftIO
            ( withMPD
            $ MPD.consume (maybe False (not . MPD.stConsume) (status s))
            )
        continue =<< rebuildStatus s
      EvKey (KChar 'x') [] -> do
        _ <- liftIO
          (withMPD $ MPD.crossfade
            ( (\case
                0 -> 5
                _ -> 0
              )
            $ maybe 0 MPD.stXFadeWidth (status s)
            )
          )
        continue =<< rebuildStatus s
      EvKey (KChar 'r') [] -> do
        _ <- liftIO
          (withMPD $ MPD.repeat (maybe False (not . MPD.stRepeat) (status s)))
        continue =<< rebuildStatus s
      EvKey (KChar 'z') [] -> do
        _ <- liftIO
          (withMPD $ MPD.random (maybe False (not . MPD.stRandom) (status s)))
        continue =<< rebuildStatus s
      EvKey (KChar '/') [] ->
        continue $ s &  modeL .~ ExMode
                     &  exL . exPrefixL .~ FSearch
                     &  exL . searchDirectionL .~ True
                     &  focusL .  focExL .~ True
      EvKey (KChar '?') [] ->
        continue $ s &  modeL .~ ExMode
                     &  exL . exPrefixL .~ BSearch
                     &  exL . searchDirectionL .~ False
                     &  focusL .  focExL .~ True
      EvKey (KChar ':') [] ->
        continue $ s &  modeL .~ ExMode
                     &  exL . exPrefixL .~ Cmd
                     &  focusL .  focExL .~ True
      EvKey (KChar '.') [] -> do
        _    <- liftIO (withMPD MPD.next)
        continue =<< rebuildStatus s
      EvKey (KChar ',') [] -> do
        _    <- liftIO (withMPD MPD.previous)
        continue =<< rebuildStatus s
      EvKey (KChar ']') [] -> do
        _ <- liftIO (withMPD $ MPD.seekCur False 5)
        continue =<< rebuildStatus s
      EvKey (KChar '[') [] -> do
        _ <- liftIO (withMPD $ MPD.seekCur False (-5))
        continue =<< rebuildStatus s
      EvKey (KChar '}') [] -> do
        _ <- liftIO (withMPD $ MPD.seekCur False 30)
        continue =<< rebuildStatus s
      EvKey (KChar '{') [] -> do
        _ <- liftIO (withMPD $ MPD.seekCur False (-30))
        continue =<< rebuildStatus s
      EvKey (KChar '1') [] -> do
        _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
        continue $ s & editableL .~ False
                     & hviewL .~ QueueView
      EvKey (KChar '2') [] -> do
        _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
        continue $ s & editableL .~ False
                     & hviewL .~ LibraryView
      EvKey (KChar '3') [] -> do
        _ <- liftIO (BC.writeBChan (chan s) (Left Tick))
        continue $ s & editableL .~ False
                     & hviewL .~ PlaylistsView
      EvResize _ _ -> continue s
      _ -> case hview s of
        QueueView     -> handleEventQueue s e
        LibraryView   -> handleEventLibrary s e
        PlaylistsView -> handleEventPlaylists s e
        HelpView -> handleEventHelp s e
  (AppEvent (Left Tick)) -> continue =<< rebuildStatus s
  (AppEvent (Right (Right subs)))
    | MPD.PlaylistS `elem` subs -> continue =<< rebuildStatus =<< rebuildQueue s
    | otherwise -> continue =<< rebuildStatus s
  _ -> continue s
