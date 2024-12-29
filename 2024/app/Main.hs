module Main where

import Day22 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/22_demo.txt"
  demo2Data <- readFile "data/22_demo2.txt"
  realData <- readFile "data/22.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demo2Data
  print $ pt2 realData

