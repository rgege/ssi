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
  deriving Show
