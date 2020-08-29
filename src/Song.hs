-- |

module Song where
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           System.IO.Unsafe
import           Data.Text                     as T
import           Text.Printf                    ( printf )
{-
-- | test song, to be destroyed
soong :: MPD.Song
soong =
  unsafePerformIO $ fromJust . fromRight Nothing <$> MPD.withMPD MPD.currentSong
-}


-- | Get comma seperated metedata from tag
meta :: Text -> MPD.Metadata -> MPD.Song -> Text
meta notFound tag song = maybe
  notFound
  (T.intercalate ",")
  (MPD.toText <<$>> Map.lookup tag (MPD.sgTags song))

secondsToTime :: Integer -> Text
secondsToTime sec =
  let (minutes, seconds) = divMod sec 60
  in  T.pack $ printf "%d:%02d" minutes seconds
