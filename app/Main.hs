module Main where

import ParseWbnf
import System.Environment

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let parsed = map readExpr (lines contents)
  print parsed
