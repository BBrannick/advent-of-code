module Day09 (pt1, pt2) where

import Data.Char (digitToInt)

pt1 :: String -> Int
pt1 = sum . zipWith (*) [0..] . compress . blocks . map digitToInt . init

pt2 :: String -> Int
pt2 _ = 0

blocks :: [Int] -> [Maybe Int]
blocks = blocks' 0 True

blocks' :: Int -> Bool -> [Int] -> [Maybe Int]
blocks' _ _ [] = []
blocks' n file (x:xs) = (replicate x c) ++ blocks' n' (not file) xs
  where c = if file then Just n else Nothing
        n' = if file then n+1 else n

compress :: [Maybe Int] -> [Int]
compress mis = compress'' (0,length mis -1) mis (reverse mis)

-- Compress that checks list pointers first
compress' :: (Int,Int) -> [Maybe Int] -> [Maybe Int] -> [Int]
compress' idx@(idxHead,idxTail) fs fs' 
  | idxTail < idxHead = []
  | otherwise = compress'' idx fs fs'

compress'' :: (Int,Int) -> [Maybe Int] -> [Maybe Int] -> [Int]
compress'' (ixHead,idxTail) ns@(Nothing:_) (Nothing:fs) = compress' (ixHead,idxTail-1) ns fs
compress'' (ixHead,idxTail) ((Just n):ns) fs = n:(compress' (ixHead+1,idxTail) ns fs)
compress'' (ixHead,idxTail) (Nothing:ns) ((Just f):fs) = f:(compress' (ixHead+1,idxTail-1) ns fs)
compress'' _ _ _ = []
