-- |

module Song where
import qualified Network.MPD                   as MPD
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )
import           System.IO.Unsafe
import           Data.Text                     as T
import           Text.Printf                    ( printf )

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
artist, artistSort, album, albumArtist, albumArtistSort, title, track, name, genre, date, composer, performer, comment, disc
  :: MetaQuery
artist = metaLookup MPD.Artist
artistSort = metaLookup MPD.ArtistSort
album = metaLookup MPD.Album
albumArtist = metaLookup MPD.AlbumArtist
albumArtistSort = metaLookup MPD.AlbumArtistSort
title = metaLookup MPD.Title
track = metaLookup MPD.Track
name = metaLookup MPD.Name
genre = metaLookup MPD.Genre
date = metaLookup MPD.Date
composer = metaLookup MPD.Composer
performer = metaLookup MPD.Performer
comment = metaLookup MPD.Comment
disc = metaLookup MPD.Disc

-- | Song info queries
duration :: MetaQuery

duration song =
  let (minutes, seconds) = divMod (MPD.sgLength song) 60
  in  Just $ T.pack $ printf "%d:%02d" minutes seconds
