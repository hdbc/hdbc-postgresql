module SpecificDB where
import Database.HDBC
import Database.HDBC.Sqlite3
connectDB = 
    handleSqlError (connectSqlite3 "testtmp.sql3")

