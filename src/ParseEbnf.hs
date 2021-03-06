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

whitespace :: Parser [Char]
whitespace = many $ oneOf "\n\t "

escaped :: Parser Char
escaped = char '\\' >> choice (zipWith escapedChar codes replacements)

escapedChar :: Char -> Char -> Parser Char
escapedChar code replacement = char code >> return replacement

codes :: [Char]
codes       = ['b',  'n',  'f',  'r',  't',  '\\', '\"',  '\'', '/']

replacements :: [Char]
replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '\'', '/']

parseTerminal :: Parser EbnfVal
parseTerminal = do
  whitespace
  symbol <- oneOf "\"\'"
  x <- many $ chars
  oneOf $ show symbol
  whitespace
  return $ Terminal x
  where chars = escaped <|> noneOf "\"\'"

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

--parseConcat :: Parser EbnfVal
--parseConcat = do
--  char ','
--  whitespace
--  x <- parseExpr
--  whitespace
--  y <- parseExpr
--  return $ Concat x y

--parseAltern :: Parser EbnfVal
--parseAltern = do
--  char '|'
--  whitespace
--  x <- parseExpr
--  whitespace
--  y <- parseExpr
--  return $ Altern x y

parseOption :: Parser EbnfVal
parseOption = do
  whitespace
  char '['
  whitespace
  x <- parseExpr
  whitespace
  char ']'
  whitespace
  return $ Option x

parseRepiti :: Parser EbnfVal
parseRepiti = do
  whitespace
  char '{'
  whitespace
  x <- parseExpr
  whitespace
  char '}'
  whitespace
  return $ Repiti x

parseGroup :: Parser EbnfVal
parseGroup = do
  whitespace
  char '('
  whitespace
  x <- parseExpr
  whitespace
  char ')'
  whitespace
  return $ Group x

parseExpr :: Parser EbnfVal
parseExpr =   parseConAlt
          <|> parseTerminal
          <|> parseOption
          <|> parseRepiti
          <|> parseGroup
          <|> parseRef

parseRef :: Parser EbnfVal
parseRef = do
  whitespace
  x <- parseIdenti
  whitespace
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
