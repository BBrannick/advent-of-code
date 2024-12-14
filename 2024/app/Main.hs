module Main where

import Day12 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/12_demo.txt"
  realData <- readFile "data/12.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

