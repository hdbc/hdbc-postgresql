module TestTime(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import Control.Exception
import Data.Time
import Data.Time.LocalTime
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Convertible
import SpecificDB
import System.Locale
import qualified System.Time as ST

baseZT :: ZonedTime
baseZT = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T %z"))
         "1989-08-01 15:33:01 -0500"

baseUTC :: UTCTime
baseUTC = convert baseZT

basePT :: POSIXTime
basePT = convert baseZT

baseCT :: ST.ClockTime
baseCT = convert baseZT

baseCalT :: ST.CalendarTime
baseCalT = convert baseZT

rowdata t = [[SqlInt32 100, toSql t, SqlNull]]

testDTType inputdata = dbTestCase $ \dbh ->
    do run dbh ("CREATE TABLE hdbctesttime (testid INTEGER PRIMARY KEY NOT NULL, \
                \testvalue " ++ dateTimeTypeOfSqlValue value ++ ")") []
       finally (testIt dbh) (do run dbh "DROP TABLE hdbctesttime" []
                                commit dbh
                            )
    where testIt dbh =
              do run dbh "INSERT INTO hdbctesttime (testid, testvalue) VALUES (?, ?)"
                     [iToSql 5, value]
                 commit dbh
                 r <- quickQuery' dbh "SELECT testid, testvalue FROM hdbctesttime" []
                 case r of
                   [[testidsv, testvaluesv]] -> 
                       do assertEqual "testid" (5::Int) (fromSql testidsv)
                          assertEqual "testvalue" inputdata (fromSql testvaluesv)
          value = toSql inputdata

mkTest label inputdata =
    TestLabel label (testDTType inputdata)

tests = TestList [mkTest "POSIXTime" basePT]