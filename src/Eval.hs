module Eval where

import           Control.Monad.Except
import           Errors
import           Syntax

eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _)               = return val
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Float _)              = return val
eval val@(Rational _)           = return val
eval val@(Complex _)            = return val
eval val@(Bool _)               = return val
eval val@(DottedList _ _)       = return val
eval val@(Vector _)             = return val
eval val@(Character _)          = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             --, ("number?", unaryOp numPr)
             --, ("string?", unaryOp strPr)
             --, ("symbol?", unaryOp symPr)
             ]



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

--unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
--unaryOp f params = head $ map f params

--numPr, strPr, symPr :: ThrowsError LispVal -> ThrowsError LispVal
--numPr (Number _) = Bool True
--numPr _          = Bool False
--strPr (String _) = Bool True
--strPr _          = Bool False
--symPr (Atom _) = Bool True
--symPr _        = Bool False
