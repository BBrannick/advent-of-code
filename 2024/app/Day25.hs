module Day25 (pt1, pt2) where

import Data.List
import Data.List.Split
import Data.Either

type Key = [Int]
type Lock = [Int]

pt1 :: String -> Int
pt1 input = length [(k,l) | k <- keys, l <- locks, fits k l]
  where (keys,locks) = parse input

pt2 :: String -> Int
pt2 _ = 0

fits :: Key -> Lock -> Bool
fits k l = all (<6) $ zipWith (+) k l

parse :: String -> ([Key], [Lock])
parse str = partitionEithers . map parse' . splitOn [""] . lines $ str

parse' :: [String] -> Either Key Lock
parse' strs@(x:xs)
  | '#' `elem` x = Right (parse'' xs)
  | otherwise = Left . parse'' . reverse . init $ strs
parse' _ = error "unable to parse"

parse'' :: [String] -> [Int]
parse'' = map (length . takeWhile (=='#')) . transpose
