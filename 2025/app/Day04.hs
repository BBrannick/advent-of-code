module Day04 (pt1, pt2) where

import qualified Data.HashMap as M
import Data.Maybe

type Grid = M.Map (Int,Int) Char

pt1 :: String -> Int
pt1 ls = length . filter (accessible grid) $ rollCoords
  where grid = build ls
        rollCoords = M.keys $ grid

pt2 :: String -> Int
pt2 = allRolls . build

allRolls :: Grid -> Int
allRolls g
  | found == 0 = 0
  | otherwise = (length rolls) + allRolls g'
  where (rolls, g') = M.partitionWithKey (\k _ -> accessible g k) g
        found = length rolls

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

