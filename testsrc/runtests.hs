{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, FlexibleInstances
, FlexibleContexts
  #-}

module Runtests where

import Test.HUnit ((@?=), Assertion)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Assertions
import qualified Test.QuickCheck.Monadic as QM
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Database.HDBC
import Database.HDBC.PostgreSQL

import Data.List
import Data.AEq
import Data.Time
import Data.Monoid
import Data.Int
import Data.Word
import Data.UUID
import Data.Convertible
import Data.Fixed
import qualified Data.Set as S
import Control.Applicative
import Data.Decimal

import System.Environment
import System.Exit

import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B


instance Arbitrary (DecimalRaw Integer) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

instance Arbitrary UUID where
  arbitrary = fromWords
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

runInsertSelect :: (Convertible a SqlValue, Convertible SqlValue a) => PostgreConnection -> Query -> a -> IO a
runInsertSelect conn tname val = withTransaction conn $ do
      runRaw conn "drop table if exists table1"
      runRaw conn $ "create table table1 (val " <> tname <> ")"
      run conn "insert into table1 (val) values (?)" [convert val]
      s <- prepare conn "select val from table1"
      executeRaw s
      (Just [res])<- fetchRow s
      finish s
      return $ convert res

fetchAll :: (Statement s) => s -> IO [[SqlValue]]
fetchAll s = fetchAll' []
  where
    fetchAll' ac = do
      r <- fetchRow s
      case r of
        Nothing -> return ac
        Just ret -> fetchAll' (ret:ac)


runInsertMany :: PostgreConnection -> [String] -> [[SqlValue]] -> IO [[SqlValue]]
runInsertMany conn types values = withTransaction conn $ do
  runRaw conn "drop table if exists table1"
  runRaw conn $ pk $ "create table table1 (" ++ valnames ++ ")"
  s <- prepare conn $ pk $ "insert into table1(" ++ colnames ++ ") values (" ++ (intersperse ',' $ replicate (length names) '?') ++")"
  executeMany s values
  finish s
  s2 <- prepare conn $ pk $ "select " ++ colnames ++ " from table1"
  executeRaw s2
  fetchAll s2
  
  where
    pk = Query . TL.pack
    names = map (("val"++) . show) [1..length types]
    colnames = intercalate "," names
    valnames = intercalate "," $ map (\(a, b) -> a ++ " " ++ b) $ zip names types

setsEqual :: PostgreConnection -> [String] -> [[SqlValue]] -> Property
setsEqual conn types values = QM.monadicIO $ do
  ret <- QM.run $ runInsertMany conn types values
  QM.stop $ (S.fromList values) ==? (S.fromList ret)
  
preciseEqual :: (Eq a, Show a, Convertible SqlValue a, Convertible a SqlValue) => PostgreConnection -> Query -> a -> Property
preciseEqual conn tname val = QM.monadicIO $ do
  res <- QM.run $ runInsertSelect conn tname val
  QM.stop $ res ?== val


approxEqual :: (Show a, AEq a, Convertible SqlValue a, Convertible a SqlValue) => PostgreConnection -> Query -> a -> Property
approxEqual conn tname val = QM.monadicIO $ do
  res <- QM.run $ runInsertSelect conn tname val
  QM.stop $ res ?~== val

genTOD :: Gen TimeOfDay
genTOD = roundTod <$> arbitrary

genLT :: Gen LocalTime
genLT = rnd <$> arbitrary
  where
    rnd x@(LocalTime {localTimeOfDay = t}) = x {localTimeOfDay = roundTod t}

-- | Generate Text without 'NUL' symbols
genText :: Gen TL.Text
genText = TL.filter fltr <$> arbitrary
  where
    fltr '\NUL' = False         -- NULL truncates C string when pass to libpq binding.
    fltr _ = True

genUTC :: Gen UTCTime
genUTC = rnd <$> arbitrary
  where
    rnd x@(UTCTime {utctDayTime = d}) = x {utctDayTime = anyToMicro d}


-- | Strip TimeOfDay to microsecond precision
roundTod :: TimeOfDay -> TimeOfDay
roundTod x@(TimeOfDay {todSec = s}) = x {todSec = anyToMicro s}

anyToMicro :: (Fractional b, Real a) => a -> b
anyToMicro a = fromRational $ toRational ((fromRational $ toRational a) :: Micro)

testG1 :: PostgreConnection -> Test
testG1 c = testGroup "Can insert and select"
           [ testProperty "Decimal" $ \(d :: Decimal) -> preciseEqual c "decimal(400,255)" d
           , testProperty "Int32" $ \(i :: Int32) -> preciseEqual c "integer" i
           , testProperty "Int64" $ \(i :: Int64) -> preciseEqual c "bigint"  i
           , testProperty "Integer" $ \(i :: Integer) -> preciseEqual c "decimal(100,0)" i
           , testProperty "Double" $ \(d :: Double) -> approxEqual c "double precision" d
           , testProperty "Text" $ forAll genText $ \(t :: TL.Text) -> preciseEqual c "text" t
           , testProperty "ByteString" $ \(b :: B.ByteString) -> preciseEqual c "bytea" b
           , testProperty "Bool" $ \(b :: Bool) -> preciseEqual c "boolean" b
           , testProperty "UUID" $ \(u :: UUID) -> preciseEqual c "uuid" u
           , testProperty "BitField" $ \(w :: Word64) -> preciseEqual c "varbit" (SqlBitField w)
           , testProperty "UTCTime" $ forAll genUTC $ \(u :: UTCTime) -> preciseEqual c "timestamp with time zone" u
           , testProperty "Day" $ \(d :: Day) -> preciseEqual c "date" d
           , testProperty "TimeOfDay" $ forAll genTOD $ \(tod :: TimeOfDay) -> preciseEqual c "time" tod
           , testProperty "LocalTime" $ forAll genLT $ \(lt :: LocalTime) -> preciseEqual c "timestamp without time zone" lt
           , testProperty "Null" $ preciseEqual c "integer" SqlNull
           , testProperty "Maybe Integer" $ \(val :: Maybe Integer) -> preciseEqual c "decimal(100,0)" val
           , testProperty "Maybe ByteString" $ \(val :: Maybe B.ByteString) -> preciseEqual c "bytea" val
           , testProperty "Insert many numbers" $ \(x :: [(Integer, Decimal)]) -> setsEqual c
                                                                          ["decimal(100,0)", "decimal(400,255)"]
                                                                          $ map (\(i, d) ->  [convert i, convert d]) x
           , testProperty "Insert many text" $ \(x :: [(Maybe Integer, UUID, Maybe B.ByteString)]) -> setsEqual c
                                                                                                      ["decimal(100,0)", "text", "bytea"]
                                                                                                      $ map (\(i, u, b) ->  [convert i, convert u, convert b]) x
           ]

testAffectedRows :: PostgreConnection -> [Int32] -> Property
testAffectedRows c is = QM.monadicIO $ do
  res <- QM.run $ withTransaction c $ do
    runRaw c "drop table if exists table1"
    runRaw c "create table table1 (val bigint)"
    runMany c "insert into table1(val) values (?)" $ map ((:[]) . convert) is

    s2 <- prepare c "update table1 set val=10"
    executeRaw s2
    res <- affectedRows s2
    finish s2
  
    return res
  QM.stop $ res ?== (genericLength is)
           
testG2 :: PostgreConnection -> Test
testG2 c = testGroup "Auxiliary functions"
           [ testProperty "affectedRows" $ testAffectedRows c ]


stmtStatus :: PostgreConnection -> Assertion
stmtStatus c = do
  runRaw c "drop table table1"
  runRaw c "create table table1 (val bigint)" -- Just for postgre 9
  s <- prepare c "select * from table1"
  statementStatus s >>= (@?= StatementNew)
  executeRaw s
  statementStatus s >>= (@?= StatementExecuted)
  _ <- fetchRow s
  statementStatus s >>= (@?= StatementFetched)
  finish s
  statementStatus s >>= (@?= StatementFinished)
  reset s
  statementStatus s >>= (@?= StatementNew)

inTransactionStatus :: PostgreConnection -> Assertion
inTransactionStatus c = do
  inTransaction c >>= (@?= False)
  withTransaction c $ do
    inTransaction c >>= (@?= True)

connStatusGood :: PostgreConnection -> Assertion
connStatusGood c = connStatus c >>= (@?= ConnOK)

connClone :: PostgreConnection -> Assertion
connClone c = do
  newc <- clone c
  connStatus newc >>= (@?= ConnOK)
  withTransaction newc $ inTransaction c >>= (@?= False)
  withTransaction c $ inTransaction newc >>= (@?= False)
  disconnect newc
  connStatus newc >>= (@?= ConnDisconnected)

checkColumnNames :: PostgreConnection -> Assertion
checkColumnNames c = do
  withTransaction c $ do
    runRaw c "drop table if exists table1"
    runRaw c "create table table1 (val1 bigint, val2 bigint, val3 bigint)"
    s <- prepare c "select val1, val2, val3 from table1"
    executeRaw s
    getColumnNames s >>= (@?= ["val1", "val2", "val3"])
    getColumnsCount s >>= (@?= 3)
    finish s
  
testG3 :: PostgreConnection -> Test
testG3 c = testGroup "Fixed tests"
           [ testCase "Statement status" $ stmtStatus c
           , testCase "inTransaction return right value" $ inTransactionStatus c
           , testCase "Connection status is good" $ connStatusGood c
           , testCase "Connection clone works" $ connClone c
           , testCase "Check driver name" $ hdbcDriverName c @?= "postgresql"
           , testCase "Check transaction support" $ dbTransactionSupport c @?= True
           , testCase "Check right column names" $ checkColumnNames c
           ]

main :: IO ()
main = do
  a <- getArgs
  case a of
    (conn:args) -> do
      c <- connectPostgreSQL $ TL.pack conn
      (flip defaultMainWithArgs) args [ testG1 c
                                      , testG2 c
                                      , testG3 c
                                      ]
      disconnect c

    _ -> do
      mapM_ putStrLn [ "Need at least one argument as connection string"
                     , "the rest will be passed as arguments to test-framework"]
      exitWith $ ExitFailure 1
