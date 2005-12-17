module Testbasics(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import System.IO

openClosedb = sqlTestCase $ 
    do dbh <- connectDB
       disconnect dbh

basicQueries = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       sExecute sth []
       hPutStr stderr "\n--2"
       --fetchRow sth >>= (putStrLn . show)--(assertEqual "row 1" (Just [Just "2"]))
       hPutStr stderr "\n--3"
       finish sth
                          )
    

tests = TestList
        [
         TestLabel "basicQueries" basicQueries,
         TestLabel "openClosedb" openClosedb
         ]
