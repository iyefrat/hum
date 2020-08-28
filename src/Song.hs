-- |

module Song where
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           System.IO.Unsafe
import           Data.Text                     as T
import           Text.Printf                    ( printf )
import qualified System.FilePath               as FilePath

-- | test song, to be destroyed
soong :: MPD.Song
soong =
  unsafePerformIO $ fromJust . fromRight Nothing <$> MPD.withMPD MPD.currentSong

type MetaQuery = MPD.Song -> Maybe Text

-- | Get comma seperated metedata from tag
metaLookup :: MPD.Metadata -> MPD.Song -> Maybe Text
metaLookup tag =
  (T.intercalate "," <$>) . (MPD.toText <<$>>) . Map.lookup tag . MPD.sgTags


-- | Song tag queries
artist, artistSort, albumArtist, albumArtistSort, title, track, name, genre, date, composer, performer, comment, disc
  :: MetaQuery

[artist, artistSort, albumArtist, albumArtistSort, title, track, name, genre, date, composer, performer, comment, disc]
  = metaLookup
    <$> [ MPD.Artist
        , MPD.ArtistSort
        , MPD.Album
        , MPD.AlbumArtist
        , MPD.AlbumArtistSort
        , MPD.Title
        , MPD.Track
        , MPD.Name
        , MPD.Genre
        , MPD.Date
        , MPD.Composer
        , MPD.Performer
        , MPD.Comment
        , MPD.Disc
        ]

-- | Song info queries
duration :: MetaQuery

duration song =
  let (minutes, seconds) = divMod (MPD.sgLength song) 60
  in  Just $ T.pack $ printf "%d:%02d" minutes seconds
