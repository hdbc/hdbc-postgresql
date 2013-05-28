{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, FlexibleInstances
, FlexibleContexts
, TemplateHaskell
  #-}

module Main where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as QP
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Assertions
import qualified Test.QuickCheck.Monadic as QM
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Database.HDBC
import Database.HDBC.PostgreSQL.Implementation
import Data.UUID
import Data.Decimal

import Control.Applicative

import Data.Derive.Arbitrary
import Data.DeriveTH

instance Arbitrary (DecimalRaw Integer) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

instance Arbitrary UUID where
  arbitrary = fromWords
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

$( derive makeArbitrary ''SqlValue )

isID :: (Eq a, Show a) => (a -> b) -> (b -> a) -> a -> QP.Result
isID to from a = a ==? (from $ to a)

sqlToNativeAndBack :: SqlValue -> Property
sqlToNativeAndBack val = QM.monadicIO $ do
  res <- QM.run $ do
    let nat = sqlValueToNative val
    case nat of
      Nothing -> return SqlNull
      Just (o, b, f) -> nativeToSqlValue b f o
  QM.stop $ res ?== val
                      

convertionTests :: Test
convertionTests = testGroup "Can convert to and back"
                  [ testProperty "Bit field" $  isID formatBits parseBit
                  -- , testProperty "UTCTime" $ isID formatUTC parseUTC
                  , testProperty "Day" $ isID formatDay parseD
                  , testProperty "TimeOfDay" $ isID formatT parseT
                  , testProperty "LocalTime" $ isID formatDT parseDT
                  , testProperty "SqlValue" sqlToNativeAndBack
                  ]

main :: IO ()
main = defaultMain [convertionTests]
