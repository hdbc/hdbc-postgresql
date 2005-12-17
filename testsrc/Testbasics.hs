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
       fetchRow sth >>= (assertEqual "row 1" (Just [Just "2"]))
       finish sth
                          )
    

tests = TestList
        [
         TestLabel "basicQueries" basicQueries,
         TestLabel "openClosedb" openClosedb
         ]
