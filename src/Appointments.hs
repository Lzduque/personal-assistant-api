module Appointments (appointmentsMsg) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)
import Data.Time (ZonedTime)
import Data.List (intercalate)


data Appointment =
  Appointment
  { userID :: String
  , name :: String
  , date :: String
  , time :: String
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)



appointments :: ZonedTime -> [Appointment]
appointments day = [
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

formatAppointment :: Appointment -> String
formatAppointment a = (name $ a) ++ " at " ++ (time $ a)

appointmentsMsg :: ZonedTime -> String
appointmentsMsg day
    | numOfAppointments == 0 = "Today you have no external appointments scheduled!"
    | otherwise = "Today you have " ++ (show $ numOfAppointments) ++ " appointment(s): " ++ as ++ "."
    where
        dayAppointments = appointments day 
        numOfAppointments = length dayAppointments
        as = intercalate ", " $ map formatAppointment dayAppointments

-- Appointments for theis user, for today
-- %R
-- same as %H:%M

-- [
--     {userID: "id_001"
--     , name: "Family Doctor"
--     , date: "2020-06-05"
--     , time: "13:30"
--     },
--     {user_id: "id_001"
--     , name: "Pick up medicine"
--     , date: "2020-06-05"
--     , time: "15:00"
--     }
-- ]