module Parser where

import           Control.Monad.Except
import           Data.Array
import           Data.Complex
import           Errors
import           Numeric
import           Syntax
import           Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "Lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

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
        _ <- char '('
        x <- try parseList <|> parseDottedList
        _ <- char ')'
        return x

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
  _ <- char '\"'
  x <- many $ noneOf ['\\', '\"'] <|> escapedChar
  _ <- char '\"'
  return $ String x

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
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
  _ <- try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseOctal :: Parser LispVal
parseOctal = do
  _ <- try $ string "#o"
  x <- many1 digit
  (return . Number . fst . head) $ readOct x

parseHexadecimal :: Parser LispVal
parseHexadecimal = do
  _ <- try $ string "#x"
  x <- many1 digit
  (return . Number . fst . head) $ readHex x

parseBinary :: Parser LispVal
parseBinary = do
  _ <- try $ string "#b"
  x <- many1 digit
  (return . Number . fst . head) $ readBin x

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  let fval = concat [x,".",y]
  (return . Float . fst . head) $ readFloat fval

parseRational :: Parser LispVal
parseRational = do
  x <- many digit
  _ <- char '/'
  y <- many digit
  let ratio = concat [x, "%", y]
  (return . Rational . read) ratio

parseComplex :: Parser LispVal
parseComplex = do
  x <- many digit
  _ <- char '+'
  y <- many digit
  _ <- char 'i'
  (return . Complex) $ (read x) :+ (read y)

parseCharacter :: Parser LispVal
parseCharacter = do
  _ <- try $ string "#\\"
  val <- try $ string "newline" <|> string "space"
    <|> do { x <- anyChar ; notFollowedBy alphaNum ; return [x]}
  return $ Character $ case val of
    "newline" -> '\n'
    "space"   -> ' '
    _         -> (head val)

parseBool :: Parser LispVal
parseBool = do
  _ <- try $ char '#'
  v <- try $ oneOf ['t', 'f']
  return $ case v of
    't' -> Bool True
    _   -> Bool False

parseList :: Parser LispVal
parseList = do
  l <- sepBy parseExpr spaces
  return $ List l

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
     _ <- char ','
     _ <- char '@'
     x <- parseExpr
     return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do
  _ <- string "'#("
  arrVal <- sepBy parseExpr spaces
  _ <- char ')'
  return $ Vector $ listArray (0, (length arrVal - 1)) arrVal

