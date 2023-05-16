module Parser where

import           Data.Array
import           Data.Complex
import           Numeric
import           Syntax
import           Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "Lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val

parseExpr :: Parser LispVal
parseExpr =
    parseString
  <|> parseAtom
  <|> try parseFloat
  <|> try parseRational
  <|> try parseComplex
  <|> parseNumber
  <|> parseCharacter
  <|> parseBool
  <|> try parseVector
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnQuote
  <|> parseUnQuoteSplicing
  <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
  char '\"'
  x <- many $ noneOf ['\\', '\"'] <|> escapedChar
  char '\"'
  return $ String x

escapedChar = do
  char '\\'
  x <- oneOf ['\\', '\"', '\t', '\b', '\n', '\r', '\f']
  return x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber =
      parseNum
  <|> parseDecimal
  <|> parseOctal
  <|> parseHexadecimal
  <|> parseBinary

parseNum :: Parser LispVal
parseNum = do
  x <- many1 digit
  (return . Number . read) x

parseDecimal :: Parser LispVal
parseDecimal = do
  try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseOctal :: Parser LispVal
parseOctal = do
  try $ string "#o"
  x <- many1 digit
  (return . Number . fst . head) $ readOct x

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
  try $ string "#x"
  x <- many1 digit
  (return . Number . fst . head) $ readHex x

parseBinary :: Parser LispVal
parseBinary = do
  try $ string "#b"
  x <- many1 digit
  (return . Number . fst . head) $ readBin x

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  let fval = concat [x,".",y]
  (return . Float . fst . head) $ readFloat fval

parseRational :: Parser LispVal
parseRational = do
  x <- many digit
  char '/'
  y <- many digit
  let ratio = concat [x, "%", y]
  (return . Rational . read) ratio

parseComplex :: Parser LispVal
parseComplex = do
  x <- many digit
  char '+'
  y <- many digit
  char 'i'
  (return . Complex) $ (read x) :+ (read y)

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  val <- try $ string "newline" <|> string "space"
    <|> do { x <- anyChar ; notFollowedBy alphaNum ; return [x]}
  return $ Character $ case val of
    "newline" -> '\n'
    "space"   -> ' '
    otherwise -> (head val)

parseBool :: Parser LispVal
parseBool = do
  char '#'
  v <- oneOf ['t', 'f']
  return $ case v of
    't' -> Bool True
    'f' -> Bool False

parseList :: Parser LispVal
parseList = do
  l <- sepBy parseExpr spaces
  return $ List l

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
     char ','
     char '@'
     x <- parseExpr
     return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do
  string "'#("
  arrVal <- sepBy parseExpr spaces
  char ')'
  return $ Vector $ listArray (0, (length arrVal - 1)) arrVal

