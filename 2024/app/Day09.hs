module Day09 (pt1, pt2) where

import Data.Char (digitToInt)

-- File id b = a File with id of length b
-- S b = a space of length b
data Block = File Int Int | Space Int deriving (Show)

pt1 :: String -> Int
pt1 = sum . zipWith (*) [0..] . compress . blocks . map digitToInt . init

pt2 :: String -> Int
pt2 = sum . zipWith (*) [0..] . blockInts . arrange . parse

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

parse :: String -> [Block]
parse = parse' 0 . map digitToInt . init

parse' :: Int -> [Int] -> [Block]
parse' _ [] = []
parse' n [c] = [File n c]
parse' n (f:s:bs) = (File n f):(Space s):(parse' (n+1) bs)

blockInts :: [Block] -> [Int]
blockInts [] = []
blockInts ((File i n):bs) = (replicate n i) ++ blockInts bs
blockInts ((Space n):bs) = (replicate n 0) ++ blockInts bs

arrange :: [Block] -> [Block]
arrange = reverse . arrange' . reverse

arrange' :: [Block] -> [Block]
arrange' [] = []
arrange' (s@(Space _):bs) = s:(arrange' bs)
arrange' (f@(File _ n):bs) = case res of
                               Just bs' -> (Space n):arrange' bs'
                               Nothing -> f:(arrange' bs)
  where res = tryInsert f bs

tryInsert :: Block -> [Block] -> Maybe [Block]
tryInsert (Space _) bs = Just bs
tryInsert _ [] = Nothing
tryInsert b@(File _ fSize) (s@(Space sSize):bs)
  | fSize <= sSize = case (tryInsert b bs) of
                       Nothing -> Just (s':b:bs)
                         where s' = Space (sSize - fSize)
                       Just bs' -> Just (s:bs')
  | otherwise = fmap (s:) (tryInsert b bs)
tryInsert b (f:fs) = fmap (f:) (tryInsert b fs)
