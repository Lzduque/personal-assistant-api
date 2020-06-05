module Weather where

import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple (setRequestMethod, setRequestHost, setRequestPath, setRequestSecure, defaultRequest, setRequestQueryString, Request, QueryItem)
import Data.Aeson (FromJSON,ToJSON,parseJSON,withObject,(.:))
import GHC.Generics (Generic)
import Data.List (isInfixOf)

data Coord =
  Coord
  { lon :: Float
  , lat :: Float
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Weather =
  Weather
  { weatherId :: Int
  , weatherMain :: String
  , description :: String
  , icon :: String
  } deriving (Show, Generic, Eq, ToJSON)

instance FromJSON Weather where 
  parseJSON = withObject "Weather" $ \weather -> do
    weatherId <- weather .: "id"
    weatherMain   <- weather .: "main"
    description   <- weather .: "description"
    icon <- weather .: "icon"
    return Weather{..}

data MainWeather =
  MainWeather
  { temp :: Float
  , feels_like :: Float
  , temp_min :: Float
  , temp_max :: Float
  , pressure :: Int
  , humidity :: Int
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Wind =
  Wind
  { speed :: Float
  , deg :: Int
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Clouds =
  Clouds
  { all :: Int
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data Sys =
  Sys
  { sysType :: Int
  , sysId :: Int
  , country :: String
  , sunrise :: Int
  , sunset :: Int
  } deriving (Show, Generic, Eq, ToJSON)

instance FromJSON Sys where 
  parseJSON = withObject "Sys" $ \sy -> do
    sysType <- sy .: "type"
    sysId   <- sy .: "id"
    country <- sy .: "country"
    sunrise <- sy .: "sunrise"
    sunset  <- sy .: "sunset"
    return Sys{..}

data WeatherInfo =
  WeatherInfo
  { coord   :: Coord
  , weather :: [Weather]
  , base    :: String
  , mainWeather    :: MainWeather         
  , visibility  :: Int
  , wind    :: Wind
  , clouds  :: Clouds
  , dt  :: Int
  , sys :: Sys
  , timezone    :: Int
  , cityId  :: Int
  , name    :: String
  , cod :: Int
  } deriving (Show, Generic, Eq, ToJSON)

instance FromJSON WeatherInfo where 
  parseJSON = withObject "WeatherInfo" $ \wi -> do
    coord <- wi .: "coord"
    weather   <- wi .: "weather"
    base <- wi .: "base"
    mainWeather <- wi .: "main"
    visibility  <- wi .: "visibility"
    wind  <- wi .: "wind"
    clouds  <- wi .: "clouds"
    dt  <- wi .: "dt"
    sys  <- wi .: "sys"
    timezone  <- wi .: "timezone"
    cityId  <- wi .: "id"
    name  <- wi .: "name"
    cod  <- wi .: "cod"
    return WeatherInfo{..}


recommendation :: Int -> Int -> String -> String
recommendation minTemp maxTemp dayDescription
    | maxTemp >=25 && dayDescription == "clear sky" = "Don't forget your sunglasses and sunscreen!"
    | minTemp >= 20 && maxTemp >=25 = "Don't forget your sunglasses and sunscreen!"
    | "rain" `isInfixOf` dayDescription = "Don't forget your umbrella!"
    | otherwise = "Have a nice day!"

weatherMsg :: WeatherInfo -> Either String String
weatherMsg  weatherInfo
    | dayDescription == "" = Left "Day description is an empty string."
    | city == "" = Left "City is an empty string."
    | recommendationMsg == "" = Left "Recommendation is an empty string."
    | otherwise = Right $ "Today there will be " ++ dayDescription ++ " in " ++ city ++ "! The temperature is going from " ++ show minTemp ++ "°C to " ++ show maxTemp ++ "°C. " ++ recommendationMsg
    where
        dayDescription = description . head . weather $ weatherInfo
        city = name $ weatherInfo
        minTemp = round . temp_min . mainWeather $ weatherInfo :: Int
        maxTemp = round . temp_max . mainWeather $ weatherInfo :: Int
        recommendationMsg = recommendation minTemp maxTemp dayDescription

apiHost :: BC.ByteString
apiHost = "api.openweathermap.org"

apiPath :: BC.ByteString
apiPath = "/data/2.5/weather"

apiQuery :: BC.ByteString -> BC.ByteString -> [QueryItem]
apiQuery key local = 
    [ ("q", Just local)
    , ("appid", Just key)
    , ("units", Just "metric")
    ]

buildRequest :: [QueryItem] -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest query host method path = setRequestMethod method
                                $ setRequestQueryString query
                                $ setRequestHost host
                                $ setRequestPath path
                                $ setRequestSecure False
                                $ defaultRequest

apiRequest :: BC.ByteString -> BC.ByteString -> Request
apiRequest key local = buildRequest (apiQuery key local) apiHost "GET" apiPath


-- api.openweathermap.org/data/2.5/weather?q={city name},{state code},{country code}&appid={your api key}&units=metric
-- api.openweathermap.org/data/2.5/weather?id={city id}&appid={your api key}&units=metric
-- Toronto id -> 6087824
-- request:
-- http://api.openweathermap.org/data/2.5/weather?id=6087824&appid=cc0e99f5f6971474f1bafc6aba7d963c
-- Request {
--   host                 = "api.openweathermap.org"
--   port                 = 80
--   secure               = False
--   requestHeaders       = []
--   path                 = "/data/2.5/weather"
--   queryString          = "?id=6087824&appid=cc0e99f5f6971474f1bafc6aba7d963c&units=metric"
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }

-- response:
-- {"coord":
--     {"lon":-0.13
--     ,"lat":51.51
--     }
--     ,"weather":[
--         {"id":803
--         ,"main":"Clouds"
--         ,"description":"broken clouds"
--         ,"icon":"04d"
--         }]
--     ,"base":"stations"
--     ,"main":
--         {"temp":14.55
--         ,"feels_like":9.34
--         ,"temp_min":13.33
--         ,"temp_max":16.67
--         ,"pressure":1001
--         ,"humidity":51
--         }
--     ,"visibility":10000
--     ,"wind":
--         {"speed":5.7
--         ,"deg":300
--         }
--     ,"clouds":
--         {"all":75
--         }
--     ,"dt":1591288110
--     ,"sys":
--         {"type":1
--         ,"id":1414
--         ,"country":"GB"
--         ,"sunrise":1591242405
--         ,"sunset":1591301482
--         }
--     ,"timezone":3600
--     ,"id":2643743
--     ,"name":"London"
--     ,"cod":200
--     }

-- getResponseStatusCode <$> response

-- to consult we get the city id, from the table
-- when we register the user, the user type city and country and with that data we look in the json object and we save the city name and the city id