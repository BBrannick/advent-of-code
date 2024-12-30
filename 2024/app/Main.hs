module Main where

import Day16 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/16_demo1.txt"
  demo2Data <- readFile "data/16_demo2.txt"
  realData <- readFile "data/16.txt"
  print $ pt1 demoData
  print $ pt1 demo2Data
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 demo2Data
  print $ pt2 realData

