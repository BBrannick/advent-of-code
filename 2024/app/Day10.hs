module Day10 (pt1, pt2) where

import Data.List (nub)
import Grid (intsFromString, inBounds, findAll)

pt1 :: String -> Int
pt1 s = sum . map (score m) $ trailheads m
  where m = intsFromString s

pt2 :: String -> Int
pt2 s = sum . map (rating m) $ trailheads m
  where m = intsFromString s

trailheads :: [[Int]] -> [(Int,Int)]
trailheads m = findAll m 0

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


