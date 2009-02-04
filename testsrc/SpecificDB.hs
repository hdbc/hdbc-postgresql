module SpecificDB where
import Database.HDBC
import Database.HDBC.PostgreSQL
import Database.HDBC.PostgreSQL.Parser(convertSQL)
import Test.HUnit

connectDB = 
    handleSqlError (do dbh <- connectPostgreSQL ""
                       run dbh "SET client_min_messages=WARNING" []
                       return dbh)

dateTimeTypeOfSqlValue :: SqlValue -> String
dateTimeTypeOfSqlValue (SqlLocalDate _) = "date"
dateTimeTypeOfSqlValue (SqlLocalTimeOfDay _) = "time without time zone"
dateTimeTypeOfSqlValue (SqlZonedLocalTimeOfDay _ _) = "time with time zone"
dateTimeTypeOfSqlValue (SqlLocalTime _) = "timestamp without time zone"
dateTimeTypeOfSqlValue (SqlZonedTime _) = "timestamp with time zone"
dateTimeTypeOfSqlValue (SqlUTCTime _) = "timestamp with time zone"
dateTimeTypeOfSqlValue (SqlDiffTime _) = "interval"
dateTimeTypeOfSqlValue (SqlPOSIXTime _) = "numeric"
dateTimeTypeOfSqlValue (SqlEpochTime _) = "integer"
dateTimeTypeOfSqlValue (SqlTimeDiff _) = "interval"
dateTimeTypeOfSqlValue _ = "text"

supportsFracTime = True
