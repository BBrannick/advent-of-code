module Day04 (pt1, pt2) where

import qualified Data.HashMap as M
import Data.Maybe

type Grid = M.Map (Int,Int) Char

pt1 :: String -> Int
pt1 ls = sizeDiff grid (clearRolls grid)
  where grid = build ls

pt2 :: String -> Int
pt2 = sum . takeWhile (>0) . mapAdjacent sizeDiff . iterate clearRolls . build

sizeDiff :: Grid -> Grid -> Int
sizeDiff m1 m2 = M.size m1 - M.size m2

{-
First approach, replaced with iterative solution
allRolls :: Grid -> Int
allRolls g
  | found == 0 = 0
  | otherwise = (length rolls) + allRolls g'
  where (rolls, g') = M.partitionWithKey (\k _ -> accessible g k) g
        found = length rolls
  -}

clearRolls :: Grid -> Grid
clearRolls g = M.filterWithKey (\k _ -> not (accessible g k)) g

accessible :: M.Map (Int,Int) Char -> (Int,Int) -> Bool
accessible g (x,y) = (<4) . length
  . catMaybes . map (flip M.lookup g) $ (nbrs (x,y))

build :: String -> M.Map (Int,Int) Char
build = M.fromList
  . filter ((=='@') . snd)
  . concatMap (\(i,l) -> zip [(i,j) | j <- [0..]] l)
  . zip [0..] . lines

nbrs :: (Int,Int) -> [(Int,Int)]
nbrs (x,y) = [(x+i,y+j) | i <- [-1..1], j <- [-1..1], (i,j) /= (0,0)]

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)

