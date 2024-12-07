module Main where

import Day07 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/07_demo.txt"
  realData <- readFile "data/07.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

