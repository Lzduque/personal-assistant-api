module Greeting (greeting) where

import Data.Time (getZonedTime, defaultTimeLocale, formatTime)

greeting :: IO (String)
greeting = do
    myTime <- getZonedTime
    let time = formatTime defaultTimeLocale "%H" myTime
    let hour = read time :: Int
    return $ if
        | hour >= 18 -> "Good night, Leticia!"
        | hour >= 12 -> "Good afternoon, Leticia!"
        | otherwise -> "Good morning, Leticia!"
