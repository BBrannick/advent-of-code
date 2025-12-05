module Day05 (pt1, pt2) where

import Data.List

pt1 :: String -> Int
pt1 ls = length . filter (\i -> any (inrange i) ranges) $ ids
  where (ranges, ids) = parse ls
        inrange i (s,e) = s <= i && i <= e

pt2 :: String -> Int
pt2 = sum . map ((+1) . uncurry (flip (-))) . normalise . fst . parse

normalise :: [(Int,Int)] -> [(Int,Int)]
normalise xs = normalise' s e starts ends
  where ((s:starts),(e:ends)) = unzip (sort xs)

normalise' :: Int -> Int -> [Int] -> [Int] -> [(Int,Int)]
normalise' x y (x':xs) (y':ys)
  | x' <= y = normalise' x (max y y') xs ys
  | otherwise = (x,y):(normalise' x' y' xs ys)
normalise' x y _ _ = [(x,y)]

--

parse :: String -> ([(Int,Int)], [Int])
parse ls = (map parse' ranges, map (read::String->Int) (tail ids))
  where (ranges,ids) = break null . lines $ ls

parse' :: String -> (Int,Int)
parse' l = ((read::String->Int) start, (read::String->Int) end)
  where (start,(_:end)) = break (=='-') l

