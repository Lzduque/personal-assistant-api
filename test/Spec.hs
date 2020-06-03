import Greeting (greeting)

import Test.Hspec
import Data.Fixed (Pico(..))
import Data.Time (UTCTime(..), fromGregorian, timeOfDayToTime, TimeOfDay(..), getCurrentTimeZone, getCurrentTime, utcToZonedTime, TimeZone(..))
import Control.Monad.IO.Class (liftIO)

mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) = UTCTime (fromGregorian year mon day) (timeOfDayToTime (TimeOfDay hour min sec))

edtTimeZone :: TimeZone
edtTimeZone = TimeZone (-240) True "EDT"

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
