module Day16 (pt1, pt2, main) where

import Grid
import Data.List (nub, union)
import Data.Maybe
import qualified Data.PQueue.Min as PQ
import qualified Data.HashMap as M
import Data.Hashable

data Dir = N | E | S | W deriving (Show,Ord,Eq,Enum)
instance Hashable Dir where
  hashWithSalt = hashUsing fromEnum
type State = (Grid Char, Coord, Dir, Coord)
type Node = (Coord,Dir)

pt1 :: String -> Int
pt1 = pathCost . parse

pt2 :: String -> Int
pt2 = length . nub . map fst . pathNodes . parse

main :: IO ()
main = do
  input <- readFile "data/16_demo1.txt"
  let state@(g,s,d,_) = parse input
      opens = PQ.singleton (0,(s,d))
      costs = M.singleton (s,d) 0
      parents = M.empty
      ns = fst $ findPath state opens costs parents
      g' = tracePath g ns
  draw g'

tracePath :: Grid Char -> [Node] -> Grid Char
tracePath g ns = setCoords g $ map (fmap (\d -> case d of 
                                           N -> '^' 
                                           E -> '>' 
                                           S -> 'v' 
                                           W -> '<')) ns

pathCost :: State -> Int
pathCost = snd . findPath'

pathNodes :: State -> [Node]
pathNodes = fst . findPath'

findPath' :: State -> ([Node],Int)
findPath' state@(_,str,d,_) = findPath state opens costs parents
  where
    opens = PQ.singleton (0,(str,d))
    costs = M.singleton (str,d) 0
    parents = M.empty

findPath :: State -> PQ.MinQueue (Int,Node) -> M.Map Node Int -> M.Map Node [Node] -> ([Node],Int)
findPath s@(g,_,_,end) opens costs parents = if (fst current) == end
                                                then if maybe True ((prio <) . fst) (PQ.getMin opens') -- no remaining potential equally short path
                                                        then (
                                                        traceParents parents current,
                                                        fromJust (M.lookup current costs)
                                                             ) 
                                                        else findPath s opens' costs parents
                                                else findPath s opens'' costs' parents'
  where
    ((prio,current),opens') = PQ.deleteFindMin opens
    costCurr = M.findWithDefault 0 current costs
    nbrs = neighbours g current
    (costs',parents',opens'') = foldr (\n (cs,ps,os) ->
      let oldCost = M.lookup n costs 
          newCost = costCurr + (cost current n)
          cs' = M.insert n newCost cs
          ps' = M.insertWith (if maybe False (newCost==) oldCost then union else flip const) n [current] ps
          os' = PQ.insert (newCost + (estCost n end), n) os
       in if maybe False (newCost >) oldCost
             then (cs,ps,os)
             else (cs',ps',os')
                                      ) (costs, parents, opens') nbrs

traceParents :: M.Map Node [Node] -> Node -> [Node]
traceParents m node = concat . takeWhile (not.null) . iterate (concatMap (\n -> M.findWithDefault [] n m)) $ [node]

cost :: Node -> Node -> Int
cost (_,d) (_,d') 
  | d == d' = 1
  | otherwise = 1001

estCost :: Node -> Coord -> Int
estCost ((x,y),d) (x',y') = (abs (x'-x)) + (abs (y'-y)) + turns
  where turns = case d of
                  N -> if (x' == x) then 0 else 1000
                  E -> if (y' == y) then 0 else 1000
                  _ -> 2000

neighbours :: Grid Char -> Node -> [Node]
neighbours g ((x,y),d) = spaces
  where 
    opts = [((x+1,y),E),((x-1,y),W),((x,y-1),N),((x,y+1),S)]
    opts' = filter (not . opp d . snd) opts
    spaces = filter ((/= '#') . findAt g . fst) opts'

opp :: Dir -> Dir -> Bool
opp N S = True
opp S N = True
opp E W = True
opp W E = True
opp _ _ = False

parse :: String -> State
parse s = (g, start, E, end)
  where g = fromString s
        start = fromMaybe (0,0) (find g 'S')
        end = fromMaybe (0,0) (find g 'E')

