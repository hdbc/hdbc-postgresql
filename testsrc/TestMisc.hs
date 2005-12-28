module TestMisc(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import System.IO
import Control.Exception
import Data.Char
import qualified Data.Map as Map

rowdata = 
    [[SqlInt32 0, toSql "Testing", SqlNull],
     [SqlInt32 1, toSql "Foo", SqlInt32 5],
     [SqlInt32 2, toSql "Bar", SqlInt32 9]]

colnames = ["testid", "teststring", "testint"]
alrows :: [[(String, SqlValue)]]
alrows = map (zip colnames) rowdata

setup f = dbTestCase $ \dbh ->
   do run dbh "CREATE TABLE hdbctest2 (testid INTEGER PRIMARY KEY NOT NULL, teststring TEXT, testint INTEGER)" []
      sth <- prepare dbh "INSERT INTO hdbctest2 VALUES (?, ?, ?)"
      executeMany sth rowdata
      commit dbh
      finally (f dbh)
              (do run dbh "DROP TABLE hdbctest2" []
                  commit dbh
              )

cloneTest dbh a =
    do dbh2 <- clone dbh
       finally (handleSqlError (a dbh2))
               (handleSqlError (disconnect dbh2))

testgetColumnNames = setup $ \dbh ->
   do sth <- prepare dbh "SELECT * from hdbctest2"
      execute sth []
      cols <- getColumnNames sth
      finish sth
      ["testid", "teststring", "testint"] @=? map (map toLower) cols

testquickQuery = setup $ \dbh ->
    do results <- quickQuery dbh "SELECT * from hdbctest2 ORDER BY testid" []
       rowdata @=? results

testfetchRowAL = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid" 
       execute sth []
       fetchRowAL sth >>= (Just (head alrows) @=?)
       fetchRowAL sth >>= (Just (alrows !! 1) @=?)
       fetchRowAL sth >>= (Just (alrows !! 2) @=?)
       fetchRowAL sth >>= (Nothing @=?)
       finish sth

testfetchRowMap = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid" 
       execute sth []
       fetchRowMap sth >>= (Just (Map.fromList $ head alrows) @=?)
       fetchRowMap sth >>= (Just (Map.fromList $ alrows !! 1) @=?)
       fetchRowMap sth >>= (Just (Map.fromList $ alrows !! 2) @=?)
       fetchRowMap sth >>= (Nothing @=?)
       finish sth

testfetchAllRowsAL = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid"
       execute sth []
       fetchAllRowsAL sth >>= (alrows @=?)

testfetchAllRowsMap = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid"
       execute sth []
       fetchAllRowsMap sth >>= (map (Map.fromList) alrows @=?)

testexception = setup $ \dbh ->
    catchSql (do prepare dbh "SELECT invalidcol FROM hdbctest2"
                 assertFailure "No exception was raised"
             )
             (\e -> return ())

testrowcount = setup $ \dbh ->
    do r <- run dbh "UPDATE hdbctest2 SET testint = 25 WHERE testid = 20" []
       assertEqual "UPDATE with no change" 0 r
       r <- run dbh "UPDATE hdbctest2 SET testint = 26 WHERE testid = 0" []
       assertEqual "UPDATE with 1 change" 1 r
       r <- run dbh "UPDATE hdbctest2 SET testint = 27 WHERE testid <> 0" []
       assertEqual "UPDATE with 2 changes" 2 r
       commit dbh
       res <- quickQuery dbh "SELECT * from hdbctest2 ORDER BY testid" []
       assertEqual "final results"
         [[SqlInt32 0, toSql "Testing", SqlInt32 26],
          [SqlInt32 1, toSql "Foo", SqlInt32 27],
          [SqlInt32 2, toSql "Bar", SqlInt32 27]] res
                                                             

testclone = setup $ \dbho -> cloneTest dbho $ \dbh ->
    do results <- quickQuery dbh "SELECT * from hdbctest2 ORDER BY testid" []
       rowdata @=? results

tests = TestList [TestLabel "getColumnNames" testgetColumnNames,
                  TestLabel "quickQuery" testquickQuery,
                  TestLabel "fetchRowAL" testfetchRowAL,
                  TestLabel "fetchRowMap" testfetchRowMap,
                  TestLabel "fetchAllRowsAL" testfetchAllRowsAL,
                  TestLabel "fetchAllRowsMap" testfetchAllRowsMap,
                  TestLabel "sql exception" testexception,
                  TestLabel "clone" testclone,
                  TestLabel "update rowcount" testrowcount]
