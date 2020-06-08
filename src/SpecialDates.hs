module SpecialDates (specialDatesMsg, SpecialDate(..), TypeOfDate(..), Notification(..)) where

import Data.Aeson (FromJSON,ToJSON)
import GHC.Generics (Generic)
import Data.List (intercalate)
import Data.Time (ZonedTime(..))
import Data.List.Split (splitOn, endsWith)

import Lib (yearFromTime)


data SpecialDate =
  SpecialDate
  { userID :: String
  , name :: String
  , originalDate :: String
  , notification :: Notification
  , typeOfDate :: TypeOfDate
  } deriving (Show, Generic, Eq, ToJSON, FromJSON)

data TypeOfDate = Anniversary | Birthday | Holiday
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Notification = OneDay | OneWeek | TwoWeeks | OneMonth
  deriving (Show, Eq, Generic, ToJSON, FromJSON)



getYear :: String -> String
getYear date = head $ splitOn "-" date

formatSpecialDate :: ZonedTime -> SpecialDate -> String
formatSpecialDate dateToday SpecialDate{name, originalDate, typeOfDate}
  | typeOfDate == Holiday = name ++ " at " ++ originalDate
  | otherwise = name ++ ", " ++ diffYears ++ " year(s) " ++ " at " ++ originalDate
  where
    originalYear = read (getYear originalDate) :: Int
    currentYear = read (yearFromTime dateToday) :: Int
    diffYears = show (currentYear - originalYear)

specialDatesMsg :: [SpecialDate] -> ZonedTime -> String
specialDatesMsg allSpecialDates dateToday
    | numOfSpecialDates == 0 = "You have no special dates coming up!"
    | otherwise = "Special dates coming up:" ++ "\n" ++ specialdates ++ "."
    where
        numOfSpecialDates = length allSpecialDates
        specialdates = intercalate ", \n" $ map (formatSpecialDate dateToday) allSpecialDates 
