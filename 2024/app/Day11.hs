module Day11 (pt1, pt2) where

import qualified Data.Map as M

pt1 :: String -> Int
pt1 = main 25

pt2 :: String -> Int
pt2 = main 75

main :: Int -> String -> Int
main x = M.foldr (+) 0 . (!!x) . iterate next' . parse

parse :: String -> M.Map Int Int
parse = toMap . map (read::String->Int) . words

toMap :: [Int] -> M.Map Int Int
toMap = M.fromListWith (+) . (flip zip) (repeat 1) 

next' :: M.Map Int Int -> M.Map Int Int
next' m = M.foldrWithKey (\k n map -> M.unionWith (+) (M.map (*n) (toMap (stones k))) map) M.empty m

stones :: Int -> [Int]
stones 0 = [1]
stones x
  | even (digits x) = split x
  | otherwise = [2024*x]

split :: Int -> [Int]
split n = unTuple (divMod n p)
  where p = 10^((digits n) `div` 2)

digits :: Int -> Int
digits = (+1) . floor . logBase 10 . fromIntegral

unTuple :: (a,a) -> [a]
unTuple (x,y) = [x,y]
