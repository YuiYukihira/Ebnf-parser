module ParseEbnf where

import Text.ParserCombinators.Parsec

data EbnfVal = Terminal String
             | Concat EbnfVal EbnfVal
             | Altern EbnfVal EbnfVal
             | Option EbnfVal
             | Repiti EbnfVal
             | Group  EbnfVal
             | Ref EbnfIden

data EbnfIden = Identi String

data EbnfDef = Define EbnfIden EbnfVal

data EbnfF = Ebnf [EbnfDef]

symbol :: Parser Char
symbol = oneOf "[]{}()<>\'\"=|.,;"

whitespace :: Parser [Char]
whitespace = many $ oneOf "\n\t "

parseTerminal :: Parser EbnfVal
parseTerminal = do
  char '\"'
  x <- many (noneOf "\"")
  char '\"'
  return $ Terminal x

parseConAlt :: Parser EbnfVal
parseConAlt =
  chainl1 (parseRef <|> parseTerminal <|> parseGroups) parseOperation

parseOperation :: Parser (EbnfVal -> EbnfVal -> EbnfVal)
parseOperation = do
  whitespace
  symbol <- char '|' <|> char ','
  whitespace
  case symbol of
    '|' -> return Altern
    ',' -> return Concat

parseGroups :: Parser EbnfVal
parseGroups =   parseGroup
            <|> parseOption
            <|> parseRepiti

parseConcat :: Parser EbnfVal
parseConcat = do
  char ','
  whitespace
  x <- parseExpr
  whitespace
  y <- parseExpr
  return $ Concat x y

parseAltern :: Parser EbnfVal
parseAltern = do
  char '|'
  whitespace
  x <- parseExpr
  whitespace
  y <- parseExpr
  return $ Altern x y

parseOption :: Parser EbnfVal
parseOption = do
  char '['
  whitespace
  x <- parseExpr
  whitespace
  char ']'
  return $ Option x

parseRepiti :: Parser EbnfVal
parseRepiti = do
  char '{'
  whitespace
  x <- parseExpr
  whitespace
  char '}'
  return $ Repiti x

parseGroup :: Parser EbnfVal
parseGroup = do
  char '('
  whitespace
  x <- parseExpr
  whitespace
  char ')'
  return $ Group x

parseExpr :: Parser EbnfVal
parseExpr =   parseRef
          <|> parseConAlt
          <|> parseTerminal
          <|> parseOption
          <|> parseRepiti
          <|> parseGroup

parseRef :: Parser EbnfVal
parseRef = do
  x <- parseIdenti
  return $ Ref x

parseIdenti :: Parser EbnfIden
parseIdenti = do
  whitespace
  head <- letter <|> digit
  tail <- many (letter <|> digit <|> char '_' <|> char '-')
  whitespace
  return $ Identi $ head:tail

parseDef :: Parser EbnfDef
parseDef = do
  whitespace
  i <- parseIdenti
  whitespace
  char '='
  whitespace
  v <- parseExpr
  whitespace
  char ';'
  whitespace
  return $ Define i v

parseExprs :: Parser EbnfF
parseExprs = do
  defs <- many parseDef
  return $ Ebnf defs

parseEbnf :: String -> EbnfF
parseEbnf input = case parse parseExprs "ebnf" input of
  Left err  -> Ebnf [Define (Identi $ "No match: ") (Terminal $ show err)]
  Right vals -> vals

showDef :: EbnfDef -> String
showDef (Define iden val) = show iden ++ ": " ++ show val ++ "\n"

showIden :: EbnfIden -> String
showIden (Identi val) = val

showVal :: EbnfVal -> String
showVal (Terminal val) = "\"" ++ val ++ "\""
showVal (Concat x y) = show x ++ " + " ++ show y
showVal (Altern x y) = show x ++ " | " ++ show y
showVal (Option x)   = "( Optional: " ++ show x ++ " )"
showVal (Repiti x)   = "( Repeatable: " ++ show x ++ " )"
showVal (Group  x)   = "( Group: " ++ show x ++ " )"
showVal (Ref    x)   = "(Ref: " ++ show x ++ ")"

showEbnf :: EbnfF -> String
showEbnf (Ebnf x) = "[\n" ++ defsToString x ++ "]"

defsToString :: [EbnfDef] -> String
defsToString [] = ""
defsToString (x:xs) = (show x) ++ (defsToString xs)

instance Show EbnfVal  where show = showVal
instance Show EbnfIden where show = showIden
instance Show EbnfDef  where show = showDef
instance Show EbnfF    where show = showEbnf
