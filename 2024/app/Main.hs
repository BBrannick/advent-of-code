module Main where

import Day13 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/13_demo.txt"
  realData <- readFile "data/13.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

