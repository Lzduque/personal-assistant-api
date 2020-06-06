module Appointments (appointmentsMsg, Appointment(..)) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)
import Data.List (intercalate)



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
    | day /= "Today" && day /= "Tomorrow" && numOfAppointments == 0 = "You have no external appointments scheduled!"
    | day /= "Today" && day /= "Tomorrow" = "You have " ++ (show $ numOfAppointments) ++ " appointment(s): " ++ as ++ "."
    | numOfAppointments == 0 = day ++ " you have no external appointments scheduled!"
    | otherwise = day ++ " you have " ++ (show $ numOfAppointments) ++ " appointment(s): " ++ as ++ "."
    where
        numOfAppointments = length allAppointments
        as = intercalate ", " $ map formatAppointment allAppointments
