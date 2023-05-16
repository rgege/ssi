module Main where

import           Eval
import           Parser
import           Syntax
import           System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head


