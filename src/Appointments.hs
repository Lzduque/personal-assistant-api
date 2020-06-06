module Appointments (appointmentsMsg, Appointment(..)) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)
import Data.Time (ZonedTime)
import Data.List (intercalate)

import Lib (dateFromTime)

data Appointment =
  Appointment
  { userID :: String
  , name :: String
  , date :: String
  , time :: String
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)



formatAppointment :: Appointment -> String
formatAppointment Appointment {name, time} = name ++ " at " ++ time

appointmentsMsg :: [Appointment] -> String -> String
appointmentsMsg allAppointments day
    | numOfAppointments == 0 = day ++ " you have no external appointments scheduled!"
    | otherwise = day ++ " you have " ++ (show $ numOfAppointments) ++ " appointment(s): " ++ as ++ "."
    where
        numOfAppointments = length allAppointments
        as = intercalate ", " $ map formatAppointment allAppointments



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
