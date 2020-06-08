module Lib (userName, userId, dateFromTime, yearFromTime) where

-- import Data.Time.Clock (getCurrentTime, utctDay)
-- import Data.Time.Calendar (toGregorian)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)



userName :: String
userName = "Leticia"

userId :: String
userId = "id_001"

-- dateToday :: IO (Integer, Int, Int) -- :: (year, month, day)
-- dateToday = getCurrentTime >>= return . toGregorian . utctDay

dateFromTime :: ZonedTime -> String
dateFromTime time = formatTime defaultTimeLocale "%Y-%m-%d" time

yearFromTime :: ZonedTime -> String
yearFromTime time = formatTime defaultTimeLocale "%Y" time
