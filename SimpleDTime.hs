module SimpleDTime (
  DTime(..),
  today,
  addMonth,
  succMonth,
  addYear) where

import Data.Time

data DTime = DTime {
  year :: Int,
  month :: Int,
  day :: Int
  } deriving (Show, Eq, Ord)

addYear :: DTime -> Int -> DTime
addYear (DTime y m d) y0 = DTime (y+y0) m d

addMonth :: DTime -> Int -> DTime
addMonth (DTime y m d) m0 =
  DTime y' m' d
  where m' = 1 + ((k-1) `mod` 12 )
        y' = y + ((k-1) `div` 12 )
        k = (m + m0)

succMonth :: DTime -> DTime
succMonth dt = addMonth (dt) 1

getGregorian ::(Integer, Int, Int) ->DTime
getGregorian (a, b, c) =
  DTime (fromInteger a) b c

-- IO cann't be pure..

today :: IO DTime
today = do
  now <- getCurrentTime
  zone <- getCurrentTimeZone
  let t = utcToLocalTime zone now
      in return $ getGregorian ( toGregorian (localDay t) )

