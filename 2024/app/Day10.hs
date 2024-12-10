module Day10 (pt1, pt2) where

import Data.Char (digitToInt)
import Data.List (nub)

pt1 :: String -> Int
pt1 s = sum . map (score m) $ trailheads m
  where m = parse s

pt2 :: String -> Int
pt2 s = sum . map (rating m) $ trailheads m
  where m = parse s

parse :: String -> [[Int]]
parse = map (map (\c -> if c == '.' then (-1) else digitToInt c)) . lines

trailheads :: [[Int]] -> [(Int,Int)]
trailheads m = [(x,y) | x <- [0..(length (head m) - 1)], y <- [0..length m - 1], m!!y!!x == 0]

score :: [[Int]] -> (Int,Int) -> Int
score m c = length . nub $ paths 1 m c

rating :: [[Int]] -> (Int,Int) -> Int
rating m c = length $ paths 1 m c

paths :: Int -> [[Int]] -> (Int,Int) -> [(Int,Int)]
paths n m (x,y) 
  | n == 9 = nextSteps
  | otherwise = concatMap (paths (n+1) m) nextSteps
    where isN = (\(x',y') -> m!!y'!!x' == n)
          s = steps m (x,y)
          nextSteps = filter isN s

steps :: [[Int]] -> (Int,Int) -> [(Int,Int)]
steps m (x,y) = filter (inBounds m) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

inBounds :: [[Int]] -> (Int,Int) -> Bool
inBounds m (x,y) = 0 <= y && y < (length m) && 0 <= x && x < (length (head m))


