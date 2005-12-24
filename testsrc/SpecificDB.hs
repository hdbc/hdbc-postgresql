module SpecificDB where
import Database.HDBC
import Database.HDBC.PostgreSQL
connectDB = 
    handleSqlError (connectPostgreSQL "")

