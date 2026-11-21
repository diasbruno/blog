module Date where

import Data.Time (defaultTimeLocale, formatTime, parseTimeOrError)
import Data.Time.Clock (DiffTime, UTCTime (UTCTime), diffTimeToPicoseconds, getCurrentTime, picosecondsToDiffTime)
import GHC.Float.RealFracMethods (truncateDoubleInteger)

fromDiffTime :: DiffTime -> Double
fromDiffTime = (* 1e-12) . fromInteger . diffTimeToPicoseconds

toDiffTime :: Double -> DiffTime
toDiffTime = picosecondsToDiffTime . fromInteger . floor . (* 1e12)

now :: IO UTCTime
now = do
    (UTCTime d t) <- getCurrentTime
    return (UTCTime d (fromInteger (truncateDoubleInteger (fromDiffTime t))))

iso8601Date :: UTCTime -> String
iso8601Date = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

fileDate :: UTCTime -> String
fileDate = formatTime defaultTimeLocale "%Y%m%d%H%M%S"

postDate :: UTCTime -> String
postDate = formatTime defaultTimeLocale "%b %d, %Y"
