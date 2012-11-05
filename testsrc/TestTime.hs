{-# LANGUAGE FlexibleContexts #-}

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
import System.Locale(defaultTimeLocale)
import Database.HDBC.Locale (iso8601DateFormat)
import qualified System.Time as ST

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b

testZonedTime :: ZonedTime
testZonedTime = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T %z"))
                 "1989-08-01 15:33:01 -0500"

testZonedTimeFrac :: ZonedTime
testZonedTimeFrac = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q %z"))
                    "1989-08-01 15:33:01.536 -0500"


rowdata t = [[SqlInt32 100, toSql t, SqlNull]]

testDTType :: (Convertible SqlValue a, Show b, Eq b) =>
    a
    -> (a -> SqlValue)
    -> (a -> b)
    -> Test
testDTType inputdata convToSqlValue toComparable = dbTestCase $ \dbh ->
    do run dbh ("CREATE TABLE hdbctesttime (testid INTEGER PRIMARY KEY NOT NULL, \
                \testvalue " ++ dateTimeTypeOfSqlValue value ++ ")") []
       finally (testIt dbh) (do commit dbh
                                run dbh "DROP TABLE hdbctesttime" []
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
                          assertEqual "testvalue"
                                (toComparable inputdata)
                                (toComparable$ fromSql testvaluesv)
          value = convToSqlValue inputdata

mkTest label inputdata convfunc toComparable =
    TestLabel label (testDTType inputdata convfunc toComparable)

tests = TestList $
    ((TestLabel "Non-frac" $ testIt testZonedTime) :
     if supportsFracTime then [TestLabel "Frac" $ testIt testZonedTimeFrac] else [])

testIt baseZonedTime = 
    TestList [mkTest "Day" baseDay toSql id,
              mkTest "TimeOfDay" baseTimeOfDay toSql id,
              mkTest "ZonedTimeOfDay" baseZonedTimeOfDay toSql id,
              mkTest "LocalTime" baseLocalTime toSql id,
              mkTest "ZonedTime" baseZonedTime toSql id,
              mkTest "UTCTime" baseUTCTime toSql id,
              mkTest "DiffTime" baseDiffTime toSql id,
              mkTest "POSIXTime" basePOSIXTime posixToSql id,
              mkTest "ClockTime" baseClockTime toSql id,
              mkTest "CalendarTime" baseCalendarTime toSql ST.toClockTime,
              mkTest "TimeDiff" baseTimeDiff toSql id
             ]
    where 
      baseDay :: Day
      baseDay = localDay baseLocalTime

      baseTimeOfDay :: TimeOfDay
      baseTimeOfDay = localTimeOfDay baseLocalTime

      baseZonedTimeOfDay :: (TimeOfDay, TimeZone)
      baseZonedTimeOfDay = fromSql (SqlZonedTime baseZonedTime)

      baseLocalTime :: LocalTime
      baseLocalTime = zonedTimeToLocalTime baseZonedTime

      baseUTCTime :: UTCTime
      baseUTCTime = convert baseZonedTime

      baseDiffTime :: NominalDiffTime
      baseDiffTime = basePOSIXTime

      basePOSIXTime :: POSIXTime
      basePOSIXTime = convert baseZonedTime

      baseTimeDiff :: ST.TimeDiff
      baseTimeDiff = convert baseDiffTime

      -- No fractional parts for these two

      baseClockTime :: ST.ClockTime
      baseClockTime = convert testZonedTime

      baseCalendarTime :: ST.CalendarTime
      baseCalendarTime = convert testZonedTime
