module Day18 (pt1,pt2) where

import Data.List.Split
import Debug.Trace
import Grid
import Data.Maybe
import qualified Data.PQueue.Min as PQ
import qualified Data.HashMap as M

pt1 :: String -> Int
pt1 input = pathCost end coords'
  where
    coords = parse input
    isDemo = (length coords) < 100
    end = if isDemo then (6,6) else (70,70)
    coords' = take (if isDemo then 12 else 1024) coords

pt2 :: String -> Int
pt2 = length

_main :: Bool -> IO ()
_main isDemo = do
  input <- readFile (if isDemo then "data/18_demo.txt" else "data/18.txt")
  let coords = parse input 
      end@(x,y) = if isDemo then (6,6) else (70,70) 
      coords' = take (if isDemo then 12 else 1024) coords
      g = setCoords (gridOf '.' (x+1,y+1)) (zip coords' (repeat '#'))
  draw g
  let (p,pc) = path end coords'
      g' = setCoords g (zip p (repeat 'O'))
  draw g'


pathCost :: Coord -> [Coord] -> Int
pathCost c blocks = snd $ path c blocks

path :: Coord -> [Coord] -> ([Coord],Int)
path (x,y) blocks = findPath g (x,y) opens costs parents
  where
    g = setCoords (gridOf '.' (x+1,y+1)) (zip blocks (repeat '#'))
    opens = PQ.singleton (0,(0,0))
    costs = M.singleton (0,0) 0
    parents = M.empty

findPath :: Grid Char -> Coord -> PQ.MinQueue (Int,Coord) -> M.Map Coord Int -> M.Map Coord Coord -> ([Coord],Int)
findPath g end opens costs parents = if current == end 
                                                then (
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
          costs' = M.insert n newCost cs
          parents' = M.insert n current ps
          opens'' = PQ.insert (newCost + (estCost n end), n) os
       in (costs', parents', opens'')
                                      ) (costs, parents, opens') nbrs'

traceParents :: M.Map Coord Coord -> Coord -> [Coord]
traceParents m n = catMaybes . takeWhile isJust $ iterate (maybe Nothing (\n -> M.lookup n m)) (Just n)

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
