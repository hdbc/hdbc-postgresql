module Testbasics(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import System.IO

openClosedb = sqlTestCase $ 
    do dbh <- connectDB
       disconnect dbh

multiFinish = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       sExecute sth []
       finish sth
       finish sth
       finish sth
                          )

basicQueries = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       sExecute sth []
       fetchRow sth >>= (assertEqual "row 1" (Just [Just "2"]))
       fetchRow sth >>= (assertEqual "last row" Nothing)
       --finish sth
                          )
    

tests = TestList
        [
         TestLabel "openClosedb" openClosedb,
         TestLabel "multiFinish" multiFinish,
         TestLabel "basicQueries" basicQueries
         ]
