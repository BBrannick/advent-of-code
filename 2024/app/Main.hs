module Main where

import Day05 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/05_demo.txt"
  realData <- readFile "data/05.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

