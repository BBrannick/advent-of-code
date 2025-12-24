module Day09 (pt1, pt2) where

import Data.List.Split

pt1 :: String -> Int
pt1 = maximum . map (uncurry square) . allPairs . map parse . lines

pt2 :: String -> Int
pt2 = length

square :: (Int, Int) -> (Int, Int) -> Int
square (x, y) (x', y') = (abs (x' - x) + 1) * (abs (y' - y) + 1)

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = (repeat x `zip` xs) ++ allPairs xs

parse :: String -> (Int, Int)
parse ls = case map (read :: String -> Int) . splitOn "," $ ls of
  (x : y : _) -> (x, y)
  _ -> error "bad parse"
