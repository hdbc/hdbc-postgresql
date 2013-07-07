{-# LANGUAGE
  OverloadedStrings
  #-}

module Database.HDBC.PostgreSQL.Parser where

import Prelude hiding (take)
import Database.HDBC.Types (Query(..), SqlError(..))

import Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.ByteString as B
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Blaze.ByteString.Builder (toByteString)

import Control.Exception (throw)
import Control.Applicative ((<$>), Alternative(..))
import Data.Monoid ((<>), mempty)

data ParseResult =
  -- | copy Text to the result
  PQuoteText T.Text
  -- | copy String to the result
  | PQuoteString String
  -- | replace with series of $1 $2 $3 and so on
  | PReplace
  deriving (Show, Eq)

normalText :: Parser [ParseResult]
normalText = (:[]) . PQuoteText <$> takeWhile1 (not . (flip elem) ['\\', '?', '-', '/', '"', '\'', '$'])

qmark :: Parser [ParseResult]
qmark = (:[]) <$> ((char '?') >> (return PReplace))

comment :: Parser [ParseResult]
comment = linecomment <|> ccomment

ccomment :: Parser [ParseResult]
ccomment = (map PQuoteText) <$> (ccomment' <?> "Inline comment")
  where
    ccomment' :: Parser [T.Text]
    ccomment' = do
      _ <- string "/*"
      c <- manyTill
           (ccomment' <|> ((:[]) <$> take 1))
           $ string "*/"
      return $ ["/*"] ++ concat c ++ ["*/"]

linecomment :: Parser [ParseResult]
linecomment = linecomment' <?> "Line comment"
  where
    linecomment' = do
      _ <- string "--"
      c <- (manyTill anyChar (endOfLine <|> endOfInput)) <?> "Body of line comment"
      return [PQuoteString "--", PQuoteString c, PQuoteString "\n"]

qidentifier :: Parser [ParseResult]
qidentifier = qidentifier' <?> "Quoted identifier parser"
  where
    qidentifier' = do
      _ <- (char '"') <?> "First double quote"
      res <- (scan False scanner) <?> "qidentifier body"
      let quotes = T.count "\"" res
      if quotes `mod` 2 == 0
        then fail "the number of quotes must be even"
        else return [PQuoteString "\"", PQuoteText res]

    scanner False '"' = Just True
    scanner False _ = Just False
    scanner True '"' = Just False
    scanner True _ = Nothing
    
literal :: Parser [ParseResult]
literal = quoteLiteral <|> dollarLiteral

data QLChar = BackQ
            | Quote
            | Other 

quoteLiteral :: Parser [ParseResult]
quoteLiteral = literal' <?> "Literal string parser"
  where
    literal' = do
      _ <- char '\'' <?> "First quote"
      res <- scan Other scanner
      let quotes = T.count "'" res
          bquotes = T.count "\\'" res
      if (quotes - bquotes) `mod` 2 == 0
        then fail "the number of quotes must be even"
        else return [PQuoteString "'", PQuoteText res]

    scanner Quote '\'' = Just Other
    scanner Quote _ = Nothing
    scanner BackQ _ = Just Other
    scanner Other '\'' = Just Quote
    scanner Other '\\' = Just BackQ
    scanner Other _ = Just Other

dollarLiteral :: Parser [ParseResult]
dollarLiteral = dollarLiteral' <?> "Dollar quoted literal string parser"
  where
    dollarLiteral' = do
      _ <- char '$'
      tag <- tagParser <?> "Tag name parser"
      _ <- char '$'
      body <- (manyTill anyChar $ (char '$' >> string tag >> char '$')) <?> "Dollar quoted string body"
      let prepost = [PQuoteString "$", PQuoteText tag, PQuoteString "$"]
      return $ prepost ++ [PQuoteString body] ++ prepost

    tagParser = do
      ret <- takeTill (== '$')
      case T.length ret of
        0 -> return ret
        _ -> if inClass ['0'..'9'] $ T.head ret
             then fail "First character must not be digit"
             else return ret
      

sqlParser :: Parser [ParseResult]
sqlParser = concat <$> (many' $ choice [ normalText
                                       , qmark
                                       , comment
                                       , qidentifier
                                       , literal
                                       , (:[]) . PQuoteString . (:[]) <$> anyChar
                                       ])


buildSqlQuery :: Query -> B.ByteString
buildSqlQuery (Query q) = case eitherResult $ parse sqlParser q of
  Left e -> throw $ SqlDriverError $ "postgresql query parser: " ++ e
  Right r -> buildBS r

buildBS :: [ParseResult] -> B.ByteString
buildBS r = toByteString $ fst $ foldl bsr (mempty, 1) r
  where
    bsr (res, n) (PQuoteText t)   = (res <> fromText t, n)
    bsr (res, n) (PQuoteString s) = (res <> fromString s, n)
    bsr (res, n)  PReplace        = (res <> fromString ('$':show n), n+1)
