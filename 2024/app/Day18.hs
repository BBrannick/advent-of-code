module Day18 (pt1,pt2) where

import Data.List
import Data.List.Split
import Grid
import Data.Maybe
import qualified Data.PQueue.Min as PQ
import qualified Data.HashMap as M

pt1 :: String -> Int
pt1 input = fromJust $ pathCost end coords'
  where
    coords = parse input
    isDemo = (length coords) < 100
    end = if isDemo then (6,6) else (70,70)
    coords' = take (if isDemo then 12 else 1024) coords

pt2 :: String -> (Int,Int)
pt2 input = findFailure end coords start
  where
    coords = parse input
    isDemo = (length coords) < 100
    end = if isDemo then (6,6) else (70,70)
    start = if isDemo then 12 else 1024

findFailure :: Coord -> [Coord] -> Int -> Coord
findFailure end allBlocks i = case path end (take (i+1) allBlocks) of
                                Nothing -> allBlocks!!i
                                Just (foundPath,_) -> findFailure end allBlocks i'
                                  where i' = i + (fromJust $ findIndex (`elem` foundPath) futureBlocks) + 1
                                        futureBlocks = drop (i+1) allBlocks


pathCost :: Coord -> [Coord] -> Maybe Int
pathCost c blocks = fmap snd $ path c blocks

path :: Coord -> [Coord] -> Maybe ([Coord],Int)
path (x,y) blocks = findPath g (x,y) opens costs parents
  where
    g = setCoords (gridOf '.' (x+1,y+1)) (zip blocks (repeat '#'))
    opens = PQ.singleton (0,(0,0))
    costs = M.singleton (0,0) 0
    parents = M.empty

findPath :: Grid Char -> Coord -> PQ.MinQueue (Int,Coord) -> M.Map Coord Int -> M.Map Coord Coord -> Maybe ([Coord],Int)
findPath _ _ PQ.Empty _ _ = Nothing
findPath g end opens costs parents = if current == end 
                                                then Just (
                                                  traceParents parents current, 
                                                  fromJust (M.lookup current costs)
                                                     ) 
                                                else findPath g end opens'' costs' parents'
  where
    ((_,current),opens') = PQ.deleteFindMin opens
    costCurr = M.findWithDefault 0 current costs
    nbrs = neighbours g current
    nbrs' = filter (\n -> maybe True (\cn -> cn > (costCurr + (cost current n))) $ M.lookup n costs) nbrs
    (costs',parents',opens'') = foldr (\n (cs,ps,os) ->
      let newCost = costCurr + (cost current n)
          cs' = M.insert n newCost cs
          ps' = M.insert n current ps
          os' = PQ.insert (newCost + (estCost n end), n) os
       in (cs', ps', os')
                                      ) (costs, parents, opens') nbrs'

traceParents :: M.Map Coord Coord -> Coord -> [Coord]
traceParents m coord = catMaybes . takeWhile isJust $ iterate (maybe Nothing (\c -> M.lookup c m)) (Just coord)

cost :: Coord -> Coord -> Int
cost _ _ = 1

estCost :: Coord -> Coord -> Int
estCost (x,y) (x',y') = (abs (x'-x)) + (abs (y'-y))

neighbours :: Grid Char -> Coord -> [Coord]
neighbours g (x,y) = spaces
  where 
    opts = filter (inBounds g) [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
    spaces = filter ((/= '#') . findAt g) opts

parse :: String -> [Coord]
parse s = map (tuple . map (read::String->Int). splitOn ",") . lines $ s

tuple :: [a] -> (a,a)
tuple (x:x':_) = (x,x')
tuple _ = error "cannot tuple fewer than 2 elems"
