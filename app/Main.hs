module Main where

import Web.Scotty (scotty, get, json, post, put, text, delete, param)
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import Data.Time (getZonedTime)
import Network.HTTP.Simple (httpLBS, getResponseStatusCode, getResponseBody)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (eitherDecode)
import Database.PostgreSQL.Simple
import Control.Applicative

import Greeting (greeting)
import Weather (weatherMsg, apiRequest, WeatherInfo)
import Appointments (appointmentsMsg, Appointment(..))
import ToDos (toDosMsg, ToDo(..))
import SpecialDates (specialDatesMsg, SpecialDate(..), TypeOfDate(..), Notification(..))
import Lib (dateFromTime)



userLocal :: BC.ByteString
userLocal = "Toronto,CA"

userID :: BC.ByteString
userID = "id_001"

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

todaysAppointments :: [Appointment]
todaysAppointments = [ Appointment
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
                                  ]

tomorrowsAppointments :: [Appointment]
tomorrowsAppointments = []

-- all to dos from now on and past ones that are pending
allToDos :: [ToDo]
allToDos = [ ToDo
                    { userID = "id_001"
                    , name = "Clean Closet"
                    , date = "2020-06-05"
                    , time = "10:30"
                    , completed = True
                    }
                , ToDo
                    { userID = "id_001"
                    , name = "Bake cookies"
                    , date = "2020-06-05"
                    , time = "14:00"
                    , completed = False
                    }
                , ToDo
                    { userID = "id_001"
                    , name = "Organize bathroom cupboards"
                    , date = "2020-06-07"
                    , time = "10:00"
                    , completed = False
                    }
                ]

-- all to dos for today on and past ones that are pending
todaysToDos :: [ToDo]
todaysToDos = [ ToDo
                            { userID = "id_001"
                            , name = "Clean Closet"
                            , date = "2020-06-05"
                            , time = "10:30"
                            , completed = True
                            }
                        , ToDo
                            { userID = "id_001"
                            , name = "Bake cookies"
                            , date = "2020-06-05"
                            , time = "14:00"
                            , completed = False
                            }
                        ]

-- all special dates coming soon (in the notification period)
soonSpecialDates :: [SpecialDate]
soonSpecialDates = [ SpecialDate
                                  { userID = "id_001"
                                  , name = "Dating Anniversary"
                                  , originalDate = "2019-07-01"
                                  , notification = OneMonth
                                  , typeOfDate = Anniversary
                                  }
                              , SpecialDate
                                  { userID = "id_001"
                                  , name = "Tim's Birthday"
                                  , originalDate = "1989-06-27"
                                  , notification = OneMonth
                                  , typeOfDate = Birthday
                                  }
                              ]

allSpecialDates :: [SpecialDate]
allSpecialDates = [ SpecialDate
                                { userID = "id_001"
                                , name = "Dating Anniversary"
                                , originalDate = "2019-07-01"
                                , notification = OneMonth
                                , typeOfDate = Anniversary
                                }
                            , SpecialDate
                                { userID = "id_001"
                                , name = "Tim's Birthday"
                                , originalDate = "1989-06-27"
                                , notification = OneMonth
                                , typeOfDate = Birthday
                                }
                            , SpecialDate
                                { userID = "id_001"
                                , name = "Mom's Birthday"
                                , originalDate = "1963-08-31"
                                , notification = OneMonth
                                , typeOfDate = Birthday
                                }
                            ]



hello :: IO Int
hello = do
  conn <- connectPostgreSQL ""
  [Only i] <- query conn "select ? + ?" (40 :: Double, 2 :: Double)
  return i

userName :: Connection -> IO [String]
userName c = query_ c "SELECT name FROM child"


main :: IO ()
main = do
  mport <- lookupEnv "PORT"
  mapiKey <- lookupEnv "OPEN_WEATHER_API_KEY"
  let 
    port = case mport of
      Just p -> read p :: Int
      Nothing -> 3000
    origin = case mport of
      Just _ -> "https://lzduque.github.io" :: String
      Nothing -> "http://localhost:3000"
    apiKey = case mapiKey of
      Just k -> BC.pack k
      Nothing -> ""
  scotty port $ do
    get "/greeting" $ do
        liftIO $ putStrLn "HI!!!!"
        -- liftIO takes greeting outside its Monad and puts it into the main Monad
        timeNow <- liftIO $ getZonedTime
        let greet = greeting timeNow "Leticia"
        json greet
    get "/weather" $ do
        liftIO $ putStrLn "Sunny day!!"
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

    get "/appointments/today" $ do
      liftIO $ putStrLn "Get appointments for Today!"
      let appointments = appointmentsMsg todaysAppointments "Today"
      json appointments
    get "/appointments/tomorrow" $ do
      liftIO $ putStrLn "Get appointments for Tomorrow!"
      let appointments = appointmentsMsg tomorrowsAppointments "Tomorrow"
      json appointments
    get "/appointments" $ do
      liftIO $ putStrLn "Get ALL appointments for user!"
      timeNow <- liftIO $ getZonedTime
      let appointments = appointmentsMsg allAppointments (dateFromTime timeNow)
      json appointments
    put "/appointments/:id" $ do
      appointmentsId <- param "id"
      text $ "Id: " <> appointmentsId
      liftIO $ putStrLn "Put changes to appointment!"
    delete "/appointments/:id" $ do
      appointmentsId <- param "id"
      text $ "Id: " <> appointmentsId
      liftIO $ putStrLn "Delete an appointment!"
    post "/appointments" $ do
      liftIO $ putStrLn "Post new appointment for user!"

    get "/todos/today" $ do
      liftIO $ putStrLn "Get todos for today!"
      let todos = toDosMsg todaysToDos "today"
      json todos
    get "/todos" $ do
      liftIO $ putStrLn "Get ALL todos for user!"
      timeNow <- liftIO $ getZonedTime
      let todos = toDosMsg allToDos (dateFromTime timeNow)
      json todos
    put "/todos/:id" $ do
      todosId <- param "id"
      text $ "Id: " <> todosId
      liftIO $ putStrLn "Put changes to to do!"
    delete "/todos/:id" $ do
      todosId <- param "id"
      text $ "Id: " <> todosId
      liftIO $ putStrLn "Delete an to do!"
    post "/todos" $ do
      liftIO $ putStrLn "Post new to do for user!"

    get "/specialdates/soon" $ do
      liftIO $ putStrLn "Get special dates coming soon!"
      timeNow <- liftIO $ getZonedTime
      let specialdates = specialDatesMsg soonSpecialDates timeNow
      json specialdates
    get "/specialdates" $ do
      liftIO $ putStrLn "Get ALL special dates for user!"
      timeNow <- liftIO $ getZonedTime
      let specialdates = specialDatesMsg allSpecialDates timeNow
      json specialdates
    put "/specialdates/:id" $ do
      specialdatesId <- param "id"
      text $ "Id: " <> specialdatesId
      liftIO $ putStrLn "Put changes special date do!"
    delete "/specialdates/:id" $ do
      specialdatesId <- param "id"
      text $ "Id: " <> specialdatesId
      liftIO $ putStrLn "Delete an special date!"
    post "/specialdates" $ do
      liftIO $ putStrLn "Post new special date for user!"
