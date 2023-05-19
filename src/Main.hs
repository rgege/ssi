module Main where

import           Control.Monad
import           Errors
import           Eval
import           Parser
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled


