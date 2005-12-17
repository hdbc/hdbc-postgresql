module Testbasics(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils

openClosedb = sqlTestCase $ 
    do dbh <- connectDB
       disconnect dbh

tests = TestList
        [TestLabel "openClosedb" openClosedb]
