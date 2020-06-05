module Greeting (greeting) where

import Data.Time (ZonedTime, defaultTimeLocale, formatTime)

import Lib (userName)



zonedHour :: ZonedTime -> Int
zonedHour timeNow = read (formatTime defaultTimeLocale "%H" timeNow) :: Int

greeting :: ZonedTime -> String
greeting timeNow 
    | hour >= 18 = "Good night, " ++ userName ++ "!"
    | hour >= 12 = "Good afternoon, " ++ userName ++ "!"
    | otherwise = "Good morning, " ++ userName ++ "!"
    where
        hour = zonedHour timeNow
