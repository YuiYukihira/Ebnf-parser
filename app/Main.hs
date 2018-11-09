module Main where

import ParseEbnf
import System.Environment

main :: IO ()
-- main = do
--  [filename] <- getArgs
--  contents <- readFile filename
--  let parsed = parseEbnf contents
--  print parsed
main = do
  [filename] <- getArgs
  contents <- readFile filename
  print $ parseEbnf contents
