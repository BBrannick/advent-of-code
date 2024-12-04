module Day01 (pt1, pt2) where

import Data.List
import Data.Maybe
import qualified Data.Map as Map

pt1 :: [String] -> Int
pt1 = sum . map (uncurry diff) . sortedPairs . map parse 

diff :: Num a => a -> a -> a
diff x y = abs (x-y) 

sortedPairs :: [(Int,Int)] -> [(Int,Int)]
sortedPairs pairs = zip sortedLeft sortedRight
  where (left, right) = unzip pairs
        sortedLeft = sort left
        sortedRight = sort right

parse :: String -> (Int, Int)
parse line = (a,b)
  where a:b:_ = map (read::String->Int) $ words line 


--

pt2 :: [String] -> Int
pt2 ls = sum (map (similarity tallyMap) listL)
  where (listL, listR) = unzip $ map parse ls
        tallyMap = tally listR

similarity :: Map.Map Int Int -> Int -> Int
similarity m x = x * (fromMaybe 0 (Map.lookup x m))

tally :: Ord a => [a] -> Map.Map a Int
tally = tally' Map.empty

tally' :: Ord a => Map.Map a Int -> [a] -> Map.Map a Int
tally' m [] = m
tally' m (x:xs) = Map.insertWith (+) x 1 $ tally' m xs


