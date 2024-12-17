module Main where

import Day15 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/15_demo1.txt"
  demo2Data <- readFile "data/15_demo2.txt"
  realData <- readFile "data/15.txt"
  print $ pt1 demoData
  print $ pt1 demo2Data
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

