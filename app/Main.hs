module Main where

import Web.Scotty (scotty, get, json)
import Control.Monad.IO.Class (liftIO)

import Greeting (greeting)
import Weather (apiRequest)
import System.Environment (lookupEnv)
import Data.Time (getZonedTime)
import Network.HTTP.Simple (httpLBS, getResponseStatusCode, getResponseBody)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  mport <- lookupEnv "PORT"
  mapiKey <- lookupEnv "OPEN_WEATHER_API_KEY"
  let 
    port = case mport of
      Just p -> read p :: Int
      Nothing -> 3000
    origin = case mport of
      Just _ -> "https://lzduque.github.io"
      Nothing -> "http://localhost:3000"
    apiKey = case mapiKey of
      Just k -> BC.pack k
      Nothing -> ""
  scotty port $ do
    get "/greeting" $ do
        liftIO $ putStrLn "HI!!!!"
        -- liftIO takes greeting outside its Monad and puts it into the main Monad
        timeNow <- liftIO $ getZonedTime
        let greet = greeting timeNow
        json greet
    get "/weather" $ do
        liftIO $ putStrLn "Sunny day!!"
        liftIO $ putStrLn $ show apiKey
        -- initReq <- parseRequest apiRequest
        -- let req = initReq
        -- liftIO $ print req
        response <- httpLBS $ apiRequest apiKey
        
        let status = getResponseStatusCode response
        liftIO $ print $ apiRequest apiKey
        if status == 200
          then do
              liftIO $ print "saving request to file"
              let jsonBody = getResponseBody response
              liftIO $ L.writeFile "data.json" jsonBody
          else 
              liftIO $ print "request failed with error"

        -- json weather 
