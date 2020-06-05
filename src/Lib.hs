module Lib (userName, userId, dateToday) where

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)



userName :: String
userName = "Leticia"

userId :: String
userId = "id_001"

-- dateToday :: IO (Integer, Int, Int) -- :: (year, month, day)
-- dateToday = getCurrentTime >>= return . toGregorian . utctDay

dateToday :: ZonedTime -> String
dateToday timeNow = read (formatTime defaultTimeLocale "%Y-%m-%d" timeNow) :: String
