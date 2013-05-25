{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, FlexibleInstances
, FlexibleContexts
  #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Assertions
import qualified Test.QuickCheck.Monadic as QM
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Database.HDBC
import Database.HDBC.PostgreSQL

import Data.AEq
import Data.Time
import Data.Monoid
import Data.Int
import Data.Word
import Data.UUID
import Data.Convertible
import Data.Fixed
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
      run conn "insert into table1 (val) values ($1)" [convert val]
      s <- prepare conn "select val from table1"
      executeRaw s
      (Just [res])<- fetchRow s
      finish s
      return $ convert res

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
           , testProperty "UUID" $ \(u :: UUID) -> preciseEqual c "uuid" u
           , testProperty "BitField" $ \(w :: Word64) -> preciseEqual c "varbit" (SqlBitField w)
           , testProperty "UTCTime" $ forAll genUTC $ \(u :: UTCTime) -> preciseEqual c "timestamp with time zone" u
           , testProperty "Day" $ \(d :: Day) -> preciseEqual c "date" d
           , testProperty "TimeOfDay" $ forAll genTOD $ \(tod :: TimeOfDay) -> preciseEqual c "time" tod
           , testProperty "LocalTime" $ forAll genLT $ \(lt :: LocalTime) -> preciseEqual c "timestamp without time zone" lt
           , testProperty "Null" $ preciseEqual c "integer" SqlNull
           , testProperty "Maybe Integer" $ \(val :: Maybe Integer) -> preciseEqual c "decimal(100,0)" val
           , testProperty "Maybe ByteString" $ \(val :: Maybe B.ByteString) -> preciseEqual c "bytea" val
           ]

main :: IO ()
main = do
  a <- getArgs
  case a of
    (conn:args) -> do
      c <- connectPostgreSQL $ TL.pack conn
      runRaw c "set timezone='utc'" -- Some bug in postgresql http://stackoverflow.com/questions/16572455
      (flip defaultMainWithArgs) args [testG1 c]
      disconnect c

    _ -> do
      mapM_ putStrLn [ "Need at least one argument as connection string"
                     , "the rest will be passed as arguments to test-framework"]
      exitWith $ ExitFailure 1
