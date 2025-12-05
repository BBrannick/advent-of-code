module Day03 (pt1, pt2, pt2') where

import Data.List
import Data.Char
import Data.Ord

pt1 :: String -> Int
pt1 = sum . map (jolt2 . map digitToInt) . lines

pt2 :: String -> Int
pt2 = sum . map (joltN 12 . map digitToInt) . lines

pt2' :: String -> Int
pt2' = sum . map (joltR 12 . reverse . map digitToInt) . lines

jolt2 :: [Int] -> Int
jolt2 = jolt2' 0 0 

jolt2' :: Int -> Int -> [Int] -> Int
jolt2' cl cr [x] = (cl*10) + (max cr x)
jolt2' cl cr (x:xs)
  | x > cl = jolt2' x 0 xs
  | otherwise = jolt2' cl (max cr x) xs

joltN :: Int -> [Int] -> Int
joltN 1 xs = maximum xs
joltN n xs = (mx * 10^(n-1)) + joltN (n-1) (drop (idx+1) xs)
  where (mx,idx) = maxIndex (dropTail (n-1) xs)

-- Works on a pre-reversed list, so we're not constantly reversing in 'dropTail' and 'maxIndex'
-- Doesn't appear to be much faster, but saves some memory
joltR :: Int -> [Int] -> Int
joltR 1 xs = maximum xs
joltR n xs = (mx * 10^(n-1)) + joltR (n-1) (take (n+idx-1) xs)
  where (mx,idx) = maxIndex' (drop (n-1) xs)

--

dropTail :: Int -> [a] -> [a]
dropTail n = reverse . drop n . reverse

-- reverse because maximumBy will return last maximal element, and we want the first
maxIndex :: Ord a => [a] -> (a, Int)
maxIndex ls = maximumBy (comparing fst) . reverse $ zip ls [0..]

maxIndex' :: Ord a => [a] -> (a, Int)
maxIndex' ls = maximumBy (comparing fst) (zip ls [0..])
