module Greeting (greeting) where

import Data.Time (ZonedTime, defaultTimeLocale, formatTime)

greeting :: ZonedTime -> String
greeting timeNow 
    | hour >= 18 = "Good night, Leticia!"
    | hour >= 12 = "Good afternoon, Leticia!"
    | otherwise = "Good morning, Leticia!"
    where
        time = formatTime defaultTimeLocale "%H" timeNow
        hour = read time :: Int
