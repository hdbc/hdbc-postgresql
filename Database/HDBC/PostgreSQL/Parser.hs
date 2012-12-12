{- -*- mode: haskell; -*- 
-}
{- PostgreSQL uses $1, $2, etc. instead of ? in query strings.  So we have to
do some basic parsing on these things to fix 'em up. -}

module Database.HDBC.PostgreSQL.Parser where

data ParserState = Clear
                 | Literal Bool
                 | QIdentifier
                 | CComment Int
                 | LineComment Int

convertSQL :: String -> String
convertSQL = convertSQLAux "" 1 Clear

ungetLiteral :: Bool -> String -> (String, String)
ungetLiteral prevBackSlash = aux ""
    where aux :: String -> String -> (String, String)
          aux acc "" = (reverse acc, "")

          aux acc ('\'':'\'':xs) = aux ('\'':'\'':acc) xs

          aux acc s@('\'':'\\':xs) =
              if prevBackSlash then
                  (reverse acc, s)
              else
                  aux ('\\':'\'':acc) xs

          aux acc s@('\'':_) = (reverse acc, s)

          aux acc (x:xs) = aux (x:acc) xs

ungetQIdentifier :: String -> (String, String)
ungetQIdentifier = aux ""
    where aux :: String -> String -> (String, String)
          aux acc "" = (reverse acc, "")
          aux acc s@('"':_) = (reverse acc, s)
          aux acc (x:xs) = aux (x:acc) xs

ungetLineComment :: Int -> String -> (String, String)
ungetLineComment = aux ""
    where aux :: String -> Int -> String -> (String, String)
          aux acc _ "" = (reverse acc, "")
          aux acc 1 s@('-':'-':_) = (reverse acc, s)
          aux acc level ('-':'-':xs) = aux ('-':'-':acc) (level - 1) xs
          aux acc level (x:xs) = aux (x:acc) level xs

ungetCComment :: Int -> String -> (String, String)
ungetCComment = aux ""
    where aux :: String -> Int -> String -> (String, String)
          aux acc _ "" = (reverse acc, "")
          aux acc 1 s@('*':'/':_) = (reverse acc, s)
          aux acc level ('*':'/':xs) = aux ('/':'*':acc) (level - 1) xs
          aux acc level (x:xs) = aux (x:acc) level xs

convertSQLAux :: String -> Int -> ParserState -> String -> String
convertSQLAux acc _ Clear "" = reverse acc
convertSQLAux acc paramCount state input =
    case state of
      Clear ->
          case input of
            '?':xs -> convertSQLAux ((reverse $ show paramCount) ++ ('$':acc)) (paramCount + 1) Clear xs
            '\\':'\'':xs -> convertSQLAux ('\'':'\\':acc) paramCount (Literal True) xs
            '\'':xs -> convertSQLAux ('\'':acc) paramCount (Literal False) xs
            '"':xs -> convertSQLAux ('"':acc) paramCount QIdentifier xs
            '-':'-':xs -> convertSQLAux ('-':'-':acc) paramCount (LineComment 1) xs
            '/':'*':xs -> convertSQLAux ('*':'/':acc) paramCount (CComment 1) xs
            '\\':'?':xs -> convertSQLAux ('?':acc) paramCount Clear xs
            x:xs -> convertSQLAux (x:acc) paramCount Clear xs
            "" -> reverse acc
      Literal prevBackSlash ->
          case input of
            '\'':'\'':xs -> convertSQLAux ('\'':'\'':acc) paramCount state xs
            '\\':'\'':xs -> convertSQLAux ('\'':'\\':acc) paramCount state xs
            '\'':xs -> convertSQLAux ('\'':acc) paramCount Clear xs
            x:xs -> convertSQLAux (x:acc) paramCount state xs
            "" -> let (literal, acc') = ungetLiteral prevBackSlash acc
                 in convertSQLAux acc' paramCount Clear $ reverse literal
      QIdentifier ->
          case input of
            '"':xs -> convertSQLAux ('"':acc) paramCount Clear xs
            x:xs -> convertSQLAux (x:acc) paramCount QIdentifier xs
            "" -> let (qidentifier, acc') = ungetQIdentifier acc
                 in convertSQLAux acc' paramCount Clear $ reverse qidentifier
      LineComment level ->
          case input of
            '\n':xs -> convertSQLAux ('\n':acc) paramCount Clear xs
            '-':'-':xs -> convertSQLAux ('-':'-':acc) paramCount (LineComment $ level + 1) xs
            x:xs -> convertSQLAux (x:acc) paramCount (LineComment level) xs
            "" -> let (lineComment, acc') = ungetLineComment level acc
                 in convertSQLAux acc' paramCount Clear $ reverse lineComment
      CComment level ->
          case input of
            '*':'/':xs -> convertSQLAux ('/':'*':acc) paramCount (if level == 1 then Clear else CComment $ level - 1) xs
            '/':'*':xs -> convertSQLAux ('*':'/':acc) paramCount (CComment $ level + 1) xs
            x:xs -> convertSQLAux (x:acc) paramCount state xs
            "" -> let (cComment, acc') = ungetCComment level acc
                 in convertSQLAux acc' paramCount Clear $ reverse cComment
