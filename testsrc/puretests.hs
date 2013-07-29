{-# LANGUAGE
  OverloadedStrings
, ScopedTypeVariables
, FlexibleInstances
, FlexibleContexts
, TemplateHaskell
  #-}

module Puretests where

import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Applicative
import Data.Attoparsec.Text.Lazy
import Data.Decimal
import Data.Derive.Arbitrary
import Data.DeriveTH
import Data.List (intercalate, isInfixOf, mapAccumL, intersperse)
import Data.Monoid (mconcat)
import Data.UUID (UUID, fromWords)
import Database.HDBI
import Database.HDBI.PostgreSQL.Implementation
import Database.HDBI.PostgreSQL.Parser
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (assertFailure, Assertion, (@?=))
import Test.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Instances ()
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import qualified Test.QuickCheck.Monadic as QM
import qualified Test.QuickCheck.Property as QP

instance Arbitrary (DecimalRaw Integer) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary

instance Arbitrary UUID where
  arbitrary = fromWords
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary

instance Arbitrary BitField where
  arbitrary = BitField <$> arbitrary

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
                  [testProperty "SqlValue" sqlToNativeAndBack]

parseToLeft :: (Show a) => Parser a -> TL.Text -> Assertion
parseToLeft p t = case eitherResult $ parse p t of
  Left _ -> return ()
  Right r -> assertFailure $ "string " ++ (show t) ++ " parsed to " ++ show r

formatParseFail :: String        -- ^ Initial string
                   -> [String]   -- ^ context
                   -> String     -- ^ error message
                   -> String
formatParseFail s cts err = "string " ++ (show s)
                            ++ "\ncontext: " ++ (intercalate ", " cts)
                            ++ "\nerror: " ++ err

parseToRight :: Parser a -> TL.Text -> Assertion
parseToRight p t = case parse p t of
  Fail _ cts err -> assertFailure $ formatParseFail (TL.unpack t) cts err
  Done _ _ -> return ()

testParseQidentifier :: Assertion
testParseQidentifier = do
  parseToRight qidentifier "\"hello\""
  parseToRight qidentifier "\" efs a \""
  parseToRight qidentifier "\"\""
  parseToRight qidentifier "\"jas\"\"jas\"\"\""
  parseToRight qidentifier "\"\"\"\""
  parseToLeft qidentifier " asdf \""
  parseToLeft qidentifier "\" ijasdf "
  parseToLeft qidentifier "\"ja\"\""

testQuoteLiteral :: Assertion
testQuoteLiteral = do
  parseToRight quoteLiteral "''"
  parseToRight quoteLiteral "'aije'"
  parseToRight quoteLiteral "' asdf '''"
  parseToRight quoteLiteral "' asdfij \\''"
  parseToRight quoteLiteral "'\\'''''\\'\\''''"
  parseToRight quoteLiteral "''' asidfj '''"
  parseToRight quoteLiteral "'afe\\\\s''fa'"
  parseToLeft quoteLiteral "adf'"    -- started not from quote
  parseToLeft quoteLiteral "'aijfa\\'" -- no end quote
  parseToLeft quoteLiteral "'ssd''"

testDollarLiteral :: Assertion
testDollarLiteral = do
  parseToRight dollarLiteral "$$$$"
  parseToRight dollarLiteral "$tag$$tag$"
  parseToRight dollarLiteral "$asdf$\"jiasd$$f 'j ija\"$asdf$"
  parseToRight dollarLiteral "$tag$$$$$$ajs$$$tag$"
  parseToLeft dollarLiteral "$a$jaisdj$$" -- tag mismatch
  parseToLeft dollarLiteral "$a$ aisj\\$a $"
  parseToLeft dollarLiteral "$4sf$hello$4sf$" -- tag started with digit

testInlineComment :: Assertion
testInlineComment = do
  parseToRight ccomment "/**/"
  parseToRight ccomment "/* asdf */"
  parseToRight ccomment "/*ja e /* jasd */*/" -- inlined comment
  parseToRight ccomment "/* ij/ * iajef */"
  parseToRight ccomment "/*/*/**/*/*/"
  parseToLeft ccomment " jae */" -- no start /*
  parseToLeft ccomment "/* asdf " -- no end */

testLineComment :: Assertion
testLineComment = do
  parseToRight linecomment "--"
  parseToRight linecomment "-- jijasdf "
  parseToRight linecomment "-- jasdf sj\n"
  parseToLeft linecomment "a-- jasd" -- not started from --

buildsTo :: Query -> B.ByteString -> Assertion
buildsTo q res = (buildSqlQuery q) @?= res

testQueryBuilder :: Assertion
testQueryBuilder = do
  buildsTo "\"hello?\" ?"
    "\"hello?\" $1"
  buildsTo "'?', ?" "'?', $1"
  buildsTo "select \"a?\\?\"\"?\" from \"?\"\" \" where fld = ? and ff = 'con?t''' ?"
    "select \"a?\\?\"\"?\" from \"?\"\" \" where fld = $1 and ff = 'con?t''' $2"

parserTests :: Test
parserTests = testGroup "Parser tests"
              [ testCase "qidentifier parser" testParseQidentifier
              , testCase "quote literal string parser" testQuoteLiteral
              , testCase "dollar literal string parser" testDollarLiteral
              , testCase "inline comment parser" testInlineComment
              , testCase "line comment parser" testLineComment
              , testCase "complex query builder test" testQueryBuilder
              ]

alphabet :: [String]
alphabet = (map (:[]) "qwertyuiop[]asdfghjklzxcvbnm,. ?") ++ ["- "]

parseRightProperty :: Parser a -> String -> QP.Result
parseRightProperty p s = case parse p $ TL.pack s of
  Done _ _        -> QP.succeeded
  Fail _ cont err -> QP.failed {QP.reason = formatParseFail s cont err}

quoteLiteralGen :: Gen String
quoteLiteralGen = do
  r <- listOf $ elements $ ["''", "\\'"] ++ alphabet
  return $ "'" ++ concat r ++ "'"

dollarLiteralGen :: Gen String
dollarLiteralGen = do
  tag <- concat <$> (listOf $ elements $ alphabet ++ ["''", "\\'", "\"", "\\\"", "\"\""])
  let quotes = "$" ++ tag ++ "$"
  body <- suchThat (concat <$> (listOf
                                $ elements
                                $ alphabet ++ ["$", "''", "\\'", "\"", "\\\"", "\"\""]))
          $ not . (isInfixOf quotes) . (++ "$")
  return $ quotes ++ body ++ quotes


qidentifierGen :: Gen String
qidentifierGen = do
  body <- concat <$> (listOf $ elements $ alphabet ++ ["'", "\\'", "\"\"", "$"])
  return $ "\"" ++ body ++ "\""

inlineCommentGen :: Gen String
inlineCommentGen = do
  body <- oneof
          [ inlineCommentGen
          , concat <$> (listOf $ elements $ alphabet)
          ]
  return $ "/*" ++ body ++ "*/"

lineCommentGen :: Gen String
lineCommentGen = do
  body <- concat <$> (listOf $ elements alphabet)
  end <- elements ["", "\n"]
  return $ "--" ++ body ++ end

parameterPlaceholderGen :: Gen String
parameterPlaceholderGen = do
  (v :: Int) <- getPositive <$> arbitrary
  return $ "$" ++ (show v)

data GenS = GenQ String         -- ^ Generate string as is
          | GenR                -- ^ Generate $1, $2 sequence in this place

generateQueryPiece :: Gen (String, GenS) -- initial string and the result
generateQueryPiece = oneof
                     [ quote quoteLiteralGen
                     , quote dollarLiteralGen
                     , quote qidentifierGen
                     , quote inlineCommentGen
                     , quote parameterPlaceholderGen
                     , quote ((++ "\n") <$> lineCommentGen)
                     -- , quote $ return "\\?"
                     , return ("?", GenR)
                     ]
  where
    quote p = do
      r <- p
      return (r, GenQ r)

pieceToResult :: [(String, GenS)] -> [(String, String)]
pieceToResult p = snd $ mapAccumL accl 1 p
  where
    accl acc (x, GenQ y) = (acc, (x, y))
    accl acc (x, GenR)   = (acc+1, (x, "$" ++ show acc))

fullQueryGen :: Gen (Query, B.ByteString)
fullQueryGen = do
  (q, res) <- unzip . (intersperse (" ", " ")) . pieceToResult <$> (listOf generateQueryPiece)
  return $ (Query $ TL.pack $ concat q, toByteString $ mconcat $ map fromString res)

checkFullQueryGen :: (Query, B.ByteString) -> QP.Result
checkFullQueryGen (q, res) = (buildSqlQuery q) ?== res
    

parserProperties :: Test
parserProperties = testGroup "Parser properties"
                   [ testProperty "literal" $ forAll quoteLiteralGen $ parseRightProperty literal
                   , testProperty "dollar literal" $ forAll dollarLiteralGen $ parseRightProperty dollarLiteral
                   , testProperty "qidentifier" $ forAll qidentifierGen $ parseRightProperty qidentifier
                   , testProperty "inline comment" $ forAll inlineCommentGen $ parseRightProperty ccomment
                   , testProperty "line comment" $ forAll lineCommentGen $ parseRightProperty linecomment
                   , testProperty "whole parser" $ forAll fullQueryGen checkFullQueryGen
                   ]




main :: IO ()
main = defaultMain [ convertionTests
                   , parserTests
                   , parserProperties
                   ]
