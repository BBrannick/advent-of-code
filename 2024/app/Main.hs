module Main where

import Day19 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/19_demo.txt"
  realData <- readFile "data/19.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

