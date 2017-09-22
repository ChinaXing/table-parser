module Main where

import TableParser

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ parseJson contents
