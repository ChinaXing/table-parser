module Main where

import TableParser
import System.IO

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  contents <- getContents
  putStrLn $ parseJson contents
