module Main where

import Web.Scotty (scotty, get, json, param, text)
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Data.Time (getZonedTime)
import Network.HTTP.Simple (httpLBS, getResponseStatusCode, getResponseBody)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (eitherDecode)

import Greeting (greeting)
import Weather (weatherMsg, apiRequest, WeatherInfo)
import Appointments (appointmentsMsg, Appointment(..))


userLocal :: BC.ByteString
userLocal = "Toronto,CA"

allAppointments :: [Appointment]
allAppointments = [ Appointment
                                { userID = "id_001"
                                , name = "Family Doctor"
                                , date = "2020-06-05"
                                , time = "13:30"
                                }
                            , Appointment
                                { userID = "id_001"
                                , name = "Pick up medicine"
                                , date = "2020-06-05"
                                , time = "15:00"
                                }
                            , Appointment
                                { userID = "id_001"
                                , name = "Get package from post office"
                                , date = "2020-06-07"
                                , time = "10:00"
                                }
                            ]

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
        -- liftIO $ putStrLn $ show apiKey
        -- initReq <- parseRequest apiRequest
        -- let req = initReq
        -- liftIO $ print req
        response <- httpLBS $ apiRequest apiKey userLocal
        let status = getResponseStatusCode response
        liftIO $ print $ apiRequest apiKey userLocal
        if status == 200
          then do
              liftIO $ print "saving request to file"
              let jsonBody = getResponseBody response
              liftIO $ L.writeFile "data.json" jsonBody
              let decodedStr = eitherDecode jsonBody :: Either String WeatherInfo
              case decodedStr of
                Right weatherInfo -> do
                  liftIO $ putStrLn $ show weatherInfo
                  case weatherMsg weatherInfo of
                    Left err -> do
                      json $ err
                    Right info -> do
                      json $ info
                Left err -> json err
        else 
            liftIO $ print "request failed with error"
    get "/appointments/:day" $ do
      day <- param "day"
      text $ "Day: " <> day
      liftIO $ putStrLn "Get appointments for Today!"
      if day == "today"
        then do
          timeNow <- liftIO $ getZonedTime
          let appointments = appointmentsMsg allAppointments timeNow
          json appointments
      else
        liftIO $ print "request failed with error"
