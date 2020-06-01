module Main where

import Web.Scotty (scotty, get, json, html)
import Control.Monad.IO.Class (liftIO)

import Lib
import Greeting (greeting)
import Weather (weather)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  mport <- lookupEnv "PORT"
  let 
    port = case mport of
      Just p -> read p :: Int
      Nothing -> 3000
    origin = case mport of
      Just _ -> "https://lzduque.github.io"
      Nothing -> "http://localhost:3000"
  scotty port $ do
    get "/greeting" $ do
        liftIO $ putStrLn "HI!!!!"
        -- liftIO takes greeting outside its Monad and puts it into the main Monad
        greet <- liftIO $ greeting
        json greet
    get "/weather" $ do
        liftIO $ putStrLn "Sunny day!!"
        json weather
