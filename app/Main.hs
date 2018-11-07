module Main where

import ParseWhile

main :: IO ()
main = do
  ast <- parseFile "testfile.txt"
  print ast
