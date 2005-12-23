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
                 finally (handleSqlError (a dbh))
                         (handleSqlError (disconnect dbh))
             )

printDBInfo = handleSqlError $
    do dbh <- connectDB
       putStrLn "+-------------------------------------------------------------------------"
       putStrLn $ "| Testing HDBC database module: " ++ hdbcDriverName dbh ++
                ", bound to client: " ++ hdbcClientVer dbh
       putStrLn $ "| Proxied driver: " ++ proxiedClientName dbh ++
                ", bound to version: " ++ proxiedClientVer dbh
       putStrLn $ "| Connected to server version: " ++ dbServerVer dbh
       putStrLn "+-------------------------------------------------------------------------\n"
       disconnect dbh
