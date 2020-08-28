module UI where


import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
-- import           Brick.Widgets.Border.Style
import           Graphics.Vty.Input.Events
import           Network.MPD                    ( withMPD )
import qualified Network.MPD                   as MPD
import           Song

launch :: IO ()
launch = do
  initialState <- buildInitialState
  endState     <- defaultMain app initialState
  print endState

data HState =
  HState { status :: Maybe MPD.Status,
           currentSong :: Maybe MPD.Song,
           playlist :: [ MPD.Song ] }
  deriving (Show, Eq)

-- data ResourceName =
--  ResourceName
--  deriving (Show, Eq, Ord)
type ResourceName = String

app :: App HState e ResourceName
app = App { appDraw         = drawPlaylist
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = pure
          , appAttrMap      = const $ attrMap mempty []
          }

buildInitialState :: IO HState
buildInitialState = do
  currentSong <- fromRight Nothing <$> withMPD MPD.currentSong
  status      <- fromRight Nothing <$> (Just <<$>> withMPD MPD.status)
  playlist    <- fromRight [] <$> withMPD (MPD.playlistInfo Nothing)
  pure HState { status, currentSong, playlist }

drawSong :: HState -> [Widget ResourceName]
drawSong st =
  center
    .   borderWithLabel (str "soong")
    .   txt
    <$> [fromMaybe "draw ded." (title =<< currentSong st)]

drawPlaylist :: HState -> [Widget ResourceName]

drawPlaylist st = (: []) . center . borderWithLabel (str "soong") $ vBox
  (txt <$> (fromMaybe "" . title <$> playlist st))


handleEvent :: HState -> BrickEvent n e -> EventM n (Next HState)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey (KChar 'p') [] -> do
      st <- liftIO ((MPD.stState <$>) <$> withMPD MPD.status)
      _  <- case st of
        Left  _           -> liftIO (withMPD $ MPD.pause True)
        Right MPD.Paused  -> liftIO (withMPD $ MPD.play Nothing)
        Right MPD.Stopped -> liftIO (withMPD $ MPD.play Nothing)
        Right MPD.Playing -> liftIO (withMPD $ MPD.pause True)
      continue s
    EvKey (KChar 'j') [] -> do
      _    <- liftIO (withMPD MPD.next)
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    EvKey (KChar 'k') [] -> do
      _    <- liftIO (withMPD MPD.previous)
      song <- liftIO (withMPD MPD.currentSong)
      continue s { currentSong = fromRight Nothing song }
    _ -> continue s
  _ -> continue s

{-
TODO write generic Response handler to pring the MPDError instead of doing the thing.
TODO read over the snake guide, implement tick event to read playlist etc.
-}
