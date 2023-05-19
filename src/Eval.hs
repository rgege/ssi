module Eval where

import           Syntax

eval :: LispVal -> LispVal
eval val@(Atom _)               = val
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Float _)              = val
eval val@(Rational _)           = val
eval val@(Complex _)            = val
eval val@(Bool _)               = val
eval val@(DottedList _ _)       = val
eval val@(Vector _)             = val
eval val@(Character _)          = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("number?", unaryOp numPr)
             , ("string?", unaryOp strPr)
             , ("symbol?", unaryOp symPr) ]



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                          if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

numPr, strPr, symPr :: LispVal -> LispVal
numPr (Number _) = Bool True
numPr _          = Bool False
strPr (String _) = Bool True
strPr _          = Bool False
symPr (Atom _  ) = Bool True
symPr _          = Bool False
