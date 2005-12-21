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
                          )
    
createTable = dbTestCase (\dbh ->
    do run dbh "CREATE TABLE test1 (testname VARCHAR(20), testid INTEGER, testint INTEGER, testtext TEXT)" []
       commit dbh
                         )

dropTable = dbTestCase (\dbh ->
    do run dbh "DROP TABLE test1" []
       commit dbh
                       )

runReplace = dbTestCase (\dbh ->
    do run dbh "INSERT INTO test1 VALUES (?, ?, ?, ?)" r1
       run dbh "INSERT INTO test1 VALUES (?, ?, 2, ?)" r2
       commit dbh
       sth <- prepare dbh "SELECT * FROM test1 WHERE testname = 'runReplace' ORDER BY testid"
       sExecute sth []
       fetchRow sth >>= (assertEqual "r1" (Just r1))
       fetchRow sth >>= (assertEqual "r2" (Just [Just "runReplace", Just "2",
                                                 Just "2", Nothing]))
       fetchRow sth >>= (assertEqual "lastrow" Nothing)
                       )
    where r1 = [Just "runReplace", Just "1", Just "1234", Just "testdata"]
          r2 = [Just "runReplace", Just "2", Nothing]

executeReplace = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO test1 VALUES ('executeReplace',?,?,?)"
       sExecute sth [Just "1", Just "1234", Just "Foo"]
       sExecute sth [Just "2", Nothing, Just "Bar"]
       commit dbh
       sth <- prepare dbh "SELECT * FROM test1 WHERE testname = ? ORDER BY testid"
       sExecute sth [Just "executeReplace"]
       fetchRow sth >>= (assertEqual "r1" 
                         (Just $ map Just ["executeReplace", "1", "1234", 
                                           "Foo"]))
       fetchRow sth >>= (assertEqual "r2"
                         (Just [Just "executeReplace", Just "2", Nothing,
                                Just "Bar"]))
       fetchRow sth >>= (assertEqual "lastrow" Nothing)
                            )

executeMany = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO test1 VALUES ('multi',?,?,?)"
       sExecuteMany sth rows
       commit dbh
       sth <- prepare dbh "SELECT testid, testint, testtext FROM test1 WHERE testname = 'multi'"
       sExecute sth []
       mapM_ (\r -> fetchRow sth >>= (assertEqual "" (Just r))) rows
       fetchRow sth >>= (assertEqual "lastrow" Nothing)
                          )
    where rows = [map Just ["1", "1234", "foo"],
                  map Just ["2", "1341", "bar"],
                  [Just "3", Nothing, Nothing]]

tests = TestList
        [
         TestLabel "openClosedb" openClosedb,
         TestLabel "multiFinish" multiFinish,
         TestLabel "basicQueries" basicQueries,
         TestLabel "createTable" createTable,
         TestLabel "runReplace" runReplace,
         TestLabel "executeReplace" executeReplace,
         TestLabel "executeMany" executeMany,
         -- commit, rollback
         TestLabel "dropTable" dropTable
         ]
