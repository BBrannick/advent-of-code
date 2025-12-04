module Day03 (pt1, pt2) where

import Data.List
import Data.Char
import Data.Ord

pt1 :: String -> Int
pt1 = sum . map (maxJolt . (map digitToInt)) . lines

pt2 :: String -> Int
pt2 = sum . map (joltN 12 . map digitToInt) . lines

maxJolt :: [Int] -> Int
maxJolt = maxJolt' 0 0 

maxJolt' :: Int -> Int -> [Int] -> Int
maxJolt' cl cr [x] = (cl*10) + (max cr x)
maxJolt' cl cr (x:xs)
  | x > cl = maxJolt' x 0 xs
  | otherwise = maxJolt' cl (max cr x) xs

joltN :: Int -> [Int] -> Int
joltN 1 xs = maximum xs
joltN n xs = (max * 10^(n-1)) + joltN (n-1) (drop (idx+1) xs)
  where (max,idx) = maxIndex (dropTail (n-1) xs)

--

dropTail :: Int -> [a] -> [a]
dropTail n = reverse . drop n . reverse

-- reverse because maximumBy will return last maximal element, and we want the first
maxIndex :: Ord a => [a] -> (a, Int)
maxIndex ls = maximumBy (comparing fst) . reverse $ zip ls [0..]
