module Syntax where

import           Data.Array
import           Data.Complex

data LispVal =
    Atom String
  | String String
  | Character Char
  | Number Integer
  | Float Float
  | Rational Rational
  | Bool Bool
  | Complex (Complex Integer)
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Vector (Array Int LispVal)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name)         = name
showVal (String contents)   = "\"" ++ contents ++ "\""
showVal (Character c)       = [c]
showVal (Number contents)   = show contents
showVal (Float contents)    = show contents
showVal (Rational contents) = show contents
showVal (Complex contents)  = show contents
showVal (Bool True)         = "#t"
showVal (Bool False)        = "#f"
showVal (List contents)     = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t)    = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
showVal (Vector contents)   = show contents  -- "'#(" ++ unwordsList $ elems contents ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unwordsVector = showVal

