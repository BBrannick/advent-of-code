module Main where

import Day18 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/18_demo.txt"
  realData <- readFile "data/18.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

