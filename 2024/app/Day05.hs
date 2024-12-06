module Day05 (pt1, pt2) where

import Data.List (delete, intersect, uncons)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

type RulesMap = Map.Map Int [Int]

pt1 :: String -> Int
pt1 str = sum . map middle . filter (safe rulesMap) $ updates
  where (rulesMap, updates) = parseInput str

safe :: RulesMap -> [Int] -> Bool
safe rs = safe' rs . reverse

safe' :: RulesMap -> [Int] -> Bool
safe' _ [] = True
safe' rs (x:xs) = not (any (flip elem xs) afters) && safe' rs xs
  where afters = Map.findWithDefault [] x rs

middle :: [a] -> a
middle xs = xs!!((length xs) `div` 2)

parseInput :: String -> (RulesMap, [[Int]])
parseInput str = (rulesMap, updates)
  where (rulesStr:ls:_) = splitOn "\n\n" str
        rulesMap = mkRules rulesStr
        updates = map (map (read::String->Int) . splitOn ",") $ lines ls

mkRules :: String -> RulesMap
mkRules str = Map.fromListWith (++) (rules str)

rules :: String -> [(Int,[Int])]
rules = map rule . lines

rule :: String -> (Int,[Int])
rule = headTuple . map (read::String->Int) . splitOn "|" 

headTuple :: [a] -> (a,[a])
headTuple = fromJust . uncons

-----
-----

pt2 :: String -> Int
pt2 str = sum . map (middle . orderUpdate) $ unsafeUpdates
  where (rulesMap, updates) = parseInput str
        unsafeUpdates = filter (not.safe rulesMap) updates
        orderUpdate = buildList . filterRules rulesMap


filterRules :: RulesMap -> [Int] -> RulesMap
filterRules rm ps = Map.map (intersect (Map.keys rm')) rm'
  where rm' = Map.intersection rm (Map.fromList [(x,[]) | x <- ps])

buildList :: RulesMap -> [Int]
buildList = buildList' []

buildList' :: [Int] -> RulesMap -> [Int]
buildList' ls rm 
  | Map.null rm = ls
  | otherwise = buildList' (l:ls) rest'
    where (empties,rest) = Map.partition null rm
          l = head (Map.keys empties)
          rest' = Map.map (delete l) rest

