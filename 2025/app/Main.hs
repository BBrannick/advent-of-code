module Main where

import Day11 (pt1, pt2)

main :: IO ()
main = do
  demoData <- readFile "data/11_demo.txt"
  demoData2 <- readFile "data/11_demo2.txt"
  realData <- readFile "data/11.txt"
  print $ pt1 demoData
  print $ pt1 realData
  print $ pt2 demoData2
  print $ pt2 realData

