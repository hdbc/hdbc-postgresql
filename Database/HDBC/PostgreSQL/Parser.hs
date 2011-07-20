{- -*- mode: haskell; -*- 
{- PostgreSQL uses $1, $2, etc. instead of ? in query strings.  So we have to
do some basic parsing on these things to fix 'em up. -}

module Database.HDBC.PostgreSQL.Parser where

import Text.ParserCombinators.Parsec

escapeseq :: GenParser Char st String
escapeseq = (try $ string "''") <|>
            (try $ string "\\'")

literal :: GenParser Char st [Char]
literal = do char '\''
             s <- many (escapeseq <|> (noneOf "'" >>= (\x -> return [x])))
             char '\''
             return $ "'" ++ (concat s) ++ "'"

qidentifier :: GenParser Char st [Char]
qidentifier = do char '"'
                 s <- many (noneOf "\"")
                 char '"'
                 return $ "\"" ++ s ++ "\""

comment :: GenParser Char st [Char]
comment = ccomment <|> linecomment

ccomment :: GenParser Char st [Char]
ccomment = do string "/*"
              c <- manyTill ((try ccomment) <|> 
                             (anyChar >>= (\x -> return [x])))
                   (try (string "*/"))
              return $ "/*" ++ concat c ++ "*/"

linecomment :: GenParser Char st [Char]
linecomment = do string "--"
                 c <- many (noneOf "\n")
                 char '\n'
                 return $ "--" ++ c ++ "\n"

-- FIXME: handle pgsql dollar-quoted constants

qmark :: Num st => GenParser Char st [Char]
qmark = do char '?'
           n <- getState
           updateState (+1)
           return $ "$" ++ show n

statement :: Num st => GenParser Char st [Char]
statement = 
    do s <- many ((try qmark) <|>
                  (try comment) <|>
                  (try literal) <|>
                  (try qidentifier) <|>
                  (anyChar >>= (\x -> return [x])))
       return $ concat s

convertSQL :: String -> Either ParseError String
convertSQL input = runParser statement (1::Integer) "" input
