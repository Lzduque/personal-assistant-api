import Greeting (greeting)
import Weather (weatherMsg, WeatherInfo(..), Coord(..), Weather(..), MainWeather(..), Wind(..), Clouds(..), Sys(..))
import Appointments (appointmentsMsg, Appointment(..))
import Lib (dateFromTime)
import ToDos (toDosMsg, ToDo(..))


import Test.Hspec
import Data.Fixed (Pico)
import Data.Time (UTCTime(..), fromGregorian, timeOfDayToTime, TimeOfDay(..), utcToZonedTime, TimeZone(..))



mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, minutes, sec) = UTCTime (fromGregorian year mon day) (timeOfDayToTime (TimeOfDay hour minutes sec))

edtTimeZone :: TimeZone
edtTimeZone = TimeZone (-240) True "EDT"

emptyDescription :: WeatherInfo
emptyDescription = WeatherInfo {coord = Coord {lon = -79.42, lat = 43.7}, weather = [Weather {weatherId = 701, weatherMain = "Mist", description = "", icon = "50d"}], base = "stations", mainWeather = MainWeather {temp = 24.74, feels_like = 28.22, temp_min = 20.0, temp_max = 27.78, pressure = 1010, humidity = 94}, visibility = 9656, wind = Wind {speed = 3.1, deg = 190}, clouds = Clouds {all = 1}, dt = 1591375011, sys = Sys {sysType = 1, sysId = 941, country = "CA", sunrise = 1591349827, sunset = 1591404944}, timezone = -14400, cityId = 6167865, name = "Toronto", cod = 200}

emptyCity :: WeatherInfo
emptyCity = WeatherInfo {coord = Coord {lon = -79.42, lat = 43.7}, weather = [Weather {weatherId = 701, weatherMain = "Mist", description = "mist", icon = "50d"}], base = "stations", mainWeather = MainWeather {temp = 24.74, feels_like = 28.22, temp_min = 20.0, temp_max = 27.78, pressure = 1010, humidity = 94}, visibility = 9656, wind = Wind {speed = 3.1, deg = 190}, clouds = Clouds {all = 1}, dt = 1591375011, sys = Sys {sysType = 1, sysId = 941, country = "CA", sunrise = 1591349827, sunset = 1591404944}, timezone = -14400, cityId = 6167865, name = "", cod = 200}

testWeatherInfo :: WeatherInfo
testWeatherInfo =
    WeatherInfo 
        { coord =
             Coord 
                { lon = -79.42
                , lat = 43.7}
        , weather = 
            [ Weather 
                { weatherId = 701
                , weatherMain = "Mist"
                , description = "mist"
                , icon = "50d"}
            ]
        , base = "stations"
        , mainWeather = 
            MainWeather 
                { temp = 24.28
                , feels_like = 27.4
                , temp_min = 20.0
                , temp_max = 27.78
                , pressure = 1011
                , humidity = 93
                }
        , visibility = 6437
        , wind = 
            Wind 
                { speed = 3.1
                , deg = 220
                }
        , clouds = 
            Clouds 
                { all = 40
                }
        , dt = 1591374423
        , sys =
            Sys 
                { sysType = 1
                , sysId = 941
                , country = "CA"
                , sunrise = 1591349827
                , sunset = 1591404944
                }
        , timezone = -14400
        , cityId = 6167865
        , name = "Toronto"
        , cod = 200
        }

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
todaysAppointments = [
    Appointment
        { userID = "id_001"
        , name = "Family Doctor"
        , date = "2020-06-05"
        , time = "13:30"
        },
    Appointment
        { userID = "id_001"
        , name = "Pick up medicine"
        , date = "2020-06-05"
        , time = "15:00"
        }
    ]

tomorrowsAppointments :: [Appointment]
tomorrowsAppointments = []

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



main :: IO ()
main = hspec $ do
  describe "greeting" $ do
    --   UTC if 4 hours in front of us, EDT
    it "returns a morning greeting given a time" $ do
        let time = mkUTCTime (2020,5,2) (12, 0, 0)
        let timeZone = edtTimeZone
        let greet = greeting (utcToZonedTime timeZone time)
        greet `shouldBe` "Good morning, Leticia!"
    it "returns an afternoon greeting given a time" $ do
        let time = mkUTCTime (2020,5,2) (16, 0, 0)
        let timeZone = edtTimeZone
        let greet = greeting (utcToZonedTime timeZone time)
        greet `shouldBe` "Good afternoon, Leticia!"
    it "returns an night greeting given a time" $ do
        let time = mkUTCTime (2020,5,2) (22, 0, 0)
        let timeZone = edtTimeZone
        let greet = greeting (utcToZonedTime timeZone time)
        greet `shouldBe` "Good night, Leticia!"

  describe "weatherMsg" $ do
    it "returns the weather message for a city and the day given a weather info" $ do
        let weatherInfo = weatherMsg testWeatherInfo
        weatherInfo `shouldBe` Right "Today there will be mist in Toronto! The temperature is going from 20°C to 28°C. Don't forget your sunglasses and sunscreen!"
    it "returns an error message for a city and the day given a weather info" $ do
        let weatherInfo = weatherMsg emptyDescription
        weatherInfo `shouldBe` Left "Day description is an empty string."
    it "returns an error message for a city and the day given a weather info" $ do
        let weatherInfo = weatherMsg emptyCity
        weatherInfo `shouldBe` Left "City is an empty string."

  describe "appointmentsMsg" $ do
    it "returns the appointments message for an user today" $ do
        let appointments = appointmentsMsg todaysAppointments "Today"
        appointments `shouldBe` "Today you have 2 appointment(s): Family Doctor at 13:30, Pick up medicine at 15:00."
    it "returns no appointments message for an user that has no appointments for tomorrow" $ do
        let appointments = appointmentsMsg tomorrowsAppointments "Tomorrow" 
        appointments `shouldBe` "Tomorrow you have no external appointments scheduled!"
    it "returns all appointments for an user" $ do
        let time = mkUTCTime (2020,6,5) (12, 0, 0)
        let timeZone = edtTimeZone
        let queryTime = utcToZonedTime timeZone time
        let appointments = appointmentsMsg allAppointments (dateFromTime queryTime) 
        appointments `shouldBe` "You have 3 appointment(s): Family Doctor at 13:30, Pick up medicine at 15:00, Get package from post office at 10:00."
    -- it "put changes to a specific appointment for an user" $ do
    -- it "delete a specific appointment for an user" $ do
    -- it "create a specific appointment for an user" $ do

  describe "toDosMsg" $ do
    it "returns the toDos message for an user today" $ do
        let toDos = toDosMsg todaysToDos "today"
        toDos `shouldBe` "Your tasks for today are:\nClean Closet at 10:30, \nBake cookies at 14:00."
    it "returns all toDos for an user" $ do
        let time = mkUTCTime (2020,6,5) (12, 0, 0)
        let timeZone = edtTimeZone
        let queryTime = utcToZonedTime timeZone time
        let toDos = toDosMsg allToDos (dateFromTime queryTime) 
        toDos `shouldBe` "Your tasks are:\nClean Closet at 10:30, \nBake cookies at 14:00, \nOrganize bathroom cupboards at 10:00."
    -- it "put changes to a specific toDo for an user" $ do
    -- it "delete a specific toDo for an user" $ do
    -- it "create a specific toDo for an user" $ do


