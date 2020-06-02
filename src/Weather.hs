module Weather (apiRequest) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple (setRequestMethod, setRequestHost, setRequestHeader, setRequestPath, setRequestSecure, setRequestPort, defaultRequest, setRequestQueryString, Request, QueryItem)

-- weather ::
weather = undefined

apiHost :: BC.ByteString
apiHost = "api.openweathermap.org"

apiPath :: BC.ByteString
apiPath = "/data/2.5/weather"

apiQuery :: BC.ByteString -> [QueryItem]
apiQuery key = 
    [ ("id", Just "6087824")
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

apiRequest :: BC.ByteString -> Request
apiRequest key = buildRequest (apiQuery key) apiHost "GET" apiPath


-- api.openweathermap.org/data/2.5/weather?id={city id}&appid={your api key}&units=metric
-- Toronto id -> 6087824
-- request:
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
-- {
--   "coord": {
--     "lon": -79.51,
--     "lat": 43.6
--   },
--   "weather": [
--     {
--       "id": 800,
--       "main": "Clear",
--       "description": "clear sky",
--       "icon": "01d"
--     }
--   ],
--   "base": "stations",
--   "main": {
--     "temp": 21.14,
--     "feels_like": 19.12,
--     "temp_min": 19,
--     "temp_max": 23,
--     "pressure": 1009,
--     "humidity": 63
--   },
--   "visibility": 14484,
--   "wind": {
--     "speed": 4.6,
--     "deg": 170
--   },
--   "clouds": {
--     "all": 1
--   },
--   "dt": 1591123561,
--   "sys": {
--     "type": 1,
--     "id": 941,
--     "country": "CA",
--     "sunrise": 1591090743,
--     "sunset": 1591145612
--   },
--   "timezone": -14400,
--   "id": 6087824,
--   "name": "New Toronto",
--   "cod": 200
-- }
-- getResponseStatusCode <$> response

-- to consult we get the city id, from the table
-- when we register the user, the user type city and country and with that data we look in the json object and we save the city name and the city id