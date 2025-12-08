module Day08 (pt1, pt2) where

import Data.List
import Data.Ord
import Data.List.Split
import qualified Data.HashMap as M

type Vec = (Int,Int,Int)

pt1 :: String -> Int
pt1 = length

pt2 :: String -> Int
pt2 = length

run n ls = foldl' addConn M.empty . map snd . take n . sortBy (comparing fst) $ distances
  where pairs = allPairs . map parse . lines $ ls
        distances = zip (map (uncurry dist) pairs) pairs

addConn :: M.Map Vec [Vec] -> (Vec,Vec) -> M.Map Vec [Vec]
addConn m (v1,v2) = M.alter (doAdd v1) v2 (M.alter (doAdd v2) v1 m)
  where doAdd v = (\x -> case x of 
                           Just vs -> Just (v:vs)
                           Nothing -> Just [v])

dist :: Vec -> Vec -> Float
dist (x1,y1,z1) (x2,y2,z2) = sqrt . fromIntegral . sum . map (\(p,q) -> (p-q)^2) $ [(x1,x2),(y1,y2),(z1,z2)]

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = ((repeat x) `zip` xs) ++ allPairs xs

parse :: String -> Vec
parse str = case splitOn "," str of
              (x:y:z:_) -> (readI x, readI y, readI z)
              _ -> error "Bad string"
  where readI = read::String->Int
