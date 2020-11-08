-- |

module Hum.Song where
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Data.Text                     as T
import           Text.Printf                    ( printf )

-- | Get comma seperated metedata from tag
meta :: Text -> MPD.Metadata -> MPD.Song -> Text
meta notFound tag song = maybe
  notFound
  (T.intercalate ",")
  (MPD.toText <<$>> Map.lookup tag (MPD.sgTags song))

-- | like meta, but returns a Maybe for future use
mmeta :: MPD.Metadata -> MPD.Song -> Maybe Text
mmeta tag song =
  (T.intercalate ",") <$> (MPD.toText <<$>> Map.lookup tag (MPD.sgTags song))


secondsToTime :: Integer -> Text
secondsToTime sec =
  let (minutes, seconds) = divMod sec 60
  in  toText (printf "%d:%02d" minutes seconds :: String)
