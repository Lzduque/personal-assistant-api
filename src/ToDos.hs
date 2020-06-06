module ToDos (toDosMsg, ToDo(..)) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)
import Data.List (intercalate)



data ToDo =
  ToDo
  { userID :: String
  , name :: String
  , date :: String
  , time :: String
  , completed :: Bool
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)



formatToDo :: ToDo -> String
formatToDo ToDo {name, time} = name ++ " at " ++ time

toDosMsg :: [ToDo] -> String -> String
toDosMsg allToDos day
    | day /= "Today" && day /= "Tomorrow" && numOfToDos == 0 = "You have no external To Do's scheduled!"
    | day /= "Today" && day /= "Tomorrow" = "You have " ++ (show $ numOfToDos) ++ " To Do(s): " ++ as ++ "."
    | numOfToDos == 0 = day ++ " you have no external To Do's scheduled!"
    | otherwise = day ++ " you have " ++ (show $ numOfToDos) ++ " ToDo(s): " ++ as ++ "."
    where
        numOfToDos = length allToDos
        as = intercalate ", " $ map formatToDo allToDos
