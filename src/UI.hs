module UI where


import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
-- import           Brick.Widgets.Border.Style
import           Graphics.Vty.Input.Events
import           Network.MPD
import           Data.Either
import           Data.List
import qualified Data.Map                      as Map
import           Control.Monad.State.Strict     ( liftIO )

launch :: IO ()
launch = do
  initialState <- buildInitialState
  endState     <- defaultMain app initialState
  print endState

data State =
  State { msong :: Maybe Song }
  deriving (Show, Eq)

-- data ResourceName =
--  ResourceName
--  deriving (Show, Eq, Ord)
type ResourceName = String

app :: App State e ResourceName
app = App { appDraw         = drawSong
          , appChooseCursor = showFirstCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = pure
          , appAttrMap      = const $ attrMap mempty []
          }

buildInitialState :: IO State
buildInitialState = do
  song <- withMPD $ currentSong
  pure State { msong = fromRight Nothing song }

songTitle :: Song -> String
songTitle song =
  (\s -> case s of
      Just vals -> intercalate ", " (toString <$> vals)
      Nothing   -> "song ded."
    )
    (Map.lookup Title (sgTags song))


drawSong :: State -> [Widget ResourceName]
drawSong st =
  center
    <$> borderWithLabel (str "soong")
    <$> str
    <$> [ ((\q -> case q of
             Just s  -> s
             Nothing -> "draw ded."
           )
            (songTitle <$> (msong st))
          )
        ]

handleEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey (KChar 'p') [] -> do
      st <- liftIO ( (stState <$>) <$> (withMPD $ status))
      _ <- case st of
        Left _ -> liftIO (withMPD $ pause True)
        Right Paused -> liftIO (withMPD $ play Nothing)
        Right Stopped -> liftIO (withMPD $ play Nothing)
        Right Playing -> liftIO (withMPD $ pause True)
      continue s
    EvKey (KChar 'j') [] -> do
      _    <- liftIO (withMPD $ next)
      song <- liftIO (withMPD $ currentSong)
      continue s { msong = fromRight Nothing song }
    EvKey (KChar 'k') [] -> do
      _    <- liftIO (withMPD $ previous)
      song <- liftIO (withMPD $ currentSong)
      continue s { msong = fromRight Nothing song }
    _ -> continue s
  _ -> continue s

-- TODO write generic Response handler to pring the MPDError instead of doing the thing.
