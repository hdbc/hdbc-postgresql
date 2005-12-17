module TestUtils where
import Database.HDBC.Sqlite3
import Database.HDBC
import Test.HUnit
import Control.Exception

connectDB = 
    handleSqlError (connectSqlite3 "testtmp.sql3")
    
sqlTestCase a = 
    TestCase (handleSqlError a)

dbTestCase a =
    TestCase (do dbh <- connectDB
                 handleSqlError (a dbh)
                 handleSqlError (disconnect dbh)
             )