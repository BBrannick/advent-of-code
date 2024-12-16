module Main where

import Day14 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/14_demo.txt"
  realData <- readFile "data/14.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData
  print $ pt2 realData

