module Main where

import Day17 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/17_demo.txt"
  realData <- readFile "data/17.txt"
  print $ pt1 demoData
  print $ pt1 realData
  --print $ pt2 demoData
  print $ pt2 realData

