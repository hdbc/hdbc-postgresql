module SpecificDB where
connectDB = 
    handleSqlError (connectSqlite3 "testtmp.sql3")

