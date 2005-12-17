module TestUtils where
import Database.HDBC.Sqlite3
import Database.HDBC
import Test.HUnit

connectDB = 
    connectSqlite3 "testtmp.sql3"
    
sqlTestCase a = 
    TestCase (handleSqlError a)
