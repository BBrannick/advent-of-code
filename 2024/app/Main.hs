module Main where

import Day03 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/day_03_demo.txt"
  demo2Data <- readFile "data/day_03_demo_2.txt"
  realData <- readFile "data/day_03.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demo2Data
  print $ pt2 realData

