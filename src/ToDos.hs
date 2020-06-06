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
    | numOfToDos == 0 = "You have no tasks for " ++ day ++ "!"
    | otherwise = "Your tasks for " ++ day ++ ":" ++ "\n" ++ todos ++ "."
    where
        numOfToDos = length allToDos
        todos = intercalate ", \n" $ map formatToDo allToDos
