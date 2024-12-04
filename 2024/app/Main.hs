module Main where

import Day04 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/04_demo.txt"
  realData <- readFile "data/04.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

