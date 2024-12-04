module Day01 (pt1, pt2) where

import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

pt1 :: [String] -> Int
pt1 lines = sum $ map (uncurry diff) (sortedPairs (map parse lines))
  

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
pt2 lines = sum (map (similarity tallyMap) listL)
  where (listL, listR) = unzip $ map parse lines
        tallyMap = tally listR

similarity :: Map.Map Int Int -> Int -> Int
similarity m x = x * (fromMaybe 0 (Map.lookup x m))

tally :: Ord a => [a] -> Map.Map a Int
tally = tally' Map.empty

tally' :: Ord a => Map.Map a Int -> [a] -> Map.Map a Int
tally' m [] = m
tally' m (x:xs) = Map.insertWith (+) x 1 $ tally' m xs


