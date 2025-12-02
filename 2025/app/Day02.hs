module Day02 (pt1, pt2) where

import Data.List (nub)
import Data.List.Split

pt1 :: String -> Int
pt1 = sum . concatMap pt1Ids . parse 

pt2 :: String -> Int
pt2 = sum . concatMap pt2Ids . parse 

pt1Ids :: (Int,Int) -> [Int]
pt1Ids (x,y) = generate l h
  where l = lowestCandidate x
        h = highestCandidate y

lowestCandidate :: Int -> Int
lowestCandidate n 
  | odd d = 10^((d-1) `div` 2)
  | otherwise = left + if right > left then 1 else 0
  where (left,right) = splitNum n
        d = digits n

highestCandidate :: Int -> Int
highestCandidate n 
  | odd d = 10^((d-1) `div` 2) - 1
  | otherwise = left - if right < left then 1 else 0
  where (left,right) = splitNum n
        d = digits n

generate :: Int -> Int -> [Int]
generate x y = [repeatNum n | n <- [x..y]]

-- Part 2

pt2Ids :: (Int,Int) -> [Int]
pt2Ids (x,y) = nub . concatMap (takeWhile (<= y) . dropWhile (< x)) . concatMap candidates $ [dx..dy]
  where dx = digits x
        dy = digits y

-- todo: apologise
candidates :: Int -> [[Int]]
candidates 2 = [[x*11 | x <- [1..9]]]
candidates 3 = [[x*111 | x <- [1..9]]]
candidates 4 = [[x*101 | x <- [10..99]]]
candidates 5 = [[x*11111 | x <- [1..9]]]
candidates 6 = [[x*10101 | x <- [10..99]], [x*1001 | x <- [100..999]]]
candidates 7 = [[x*1111111 | x <- [1..9]]]
candidates 8 = [[x*10001 | x <- [1000..9999]]]
candidates 9 = [[x*1001001 | x <- [100..999]]]
candidates 10 = [[x*101010101 | x <- [10..99]], [x*100001 | x <- [10000..99999]]]
candidates _ = []

---

digits :: Int -> Int
digits = (+1) . floor . logBase 10 . fromIntegral

splitNum :: Int -> (Int,Int)
splitNum n = n `divMod` (10^(d `div` 2))
  where d = digits n

repeatNum :: Int -> Int
repeatNum n = n * (10^d + 1)
  where d = digits n

parse :: String -> [(Int,Int)]
parse = twoByTwo . map (read::String->Int) . splitOneOf ",-"

twoByTwo :: [a] -> [(a,a)]
twoByTwo (x:y:xs) = (x,y):(twoByTwo xs)
twoByTwo _ = []

