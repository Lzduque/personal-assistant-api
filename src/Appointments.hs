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



appointmentsForDay :: [Appointment] -> ZonedTime -> [Appointment]
appointmentsForDay appointments timeNow = filter (\a -> (date $ a) == day) appointments
    where
        day = dateFromTime timeNow

formatAppointment :: Appointment -> String
formatAppointment a = (name $ a) ++ " at " ++ (time $ a)

appointmentsMsg :: [Appointment] -> ZonedTime -> String
appointmentsMsg allAppointments timeNow
    | numOfAppointments == 0 = "Today you have no external appointments scheduled!"
    | otherwise = "Today you have " ++ (show $ numOfAppointments) ++ " appointment(s): " ++ as ++ "."
    where
        dayAppointments = appointmentsForDay allAppointments timeNow 
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