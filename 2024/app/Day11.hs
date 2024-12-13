module Day11 (pt1, pt2) where

import qualified Data.Map as M

pt1 :: String -> Int
pt1 = main 25

pt2 :: String -> Int
pt2 = main 75

main :: Int -> String -> Int
main x = M.foldr (+) 0 . (!!x) . iterate blink . parse

parse :: String -> M.Map Int Int
parse = tally . map (read::String->Int) . words

tally :: [Int] -> M.Map Int Int
tally = M.fromListWith (+) . (flip zip) (repeat 1) 

blink :: M.Map Int Int -> M.Map Int Int
blink m = M.foldrWithKey (\k n map -> M.unionWith (+) (M.map (*n) (tally (stones k))) map) M.empty m

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
