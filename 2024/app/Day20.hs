module Day20 (main, pt1, pt2) where

import Data.List
import Grid
import Data.Maybe
import qualified Data.PQueue.Min as PQ
import qualified Data.HashMap as M

import Control.Monad

-- Helper to visualise the results
main :: Bool -> IO ()
main isDemo = do
  let fileIn = if isDemo then "data/20_demo.txt" else "data/20.txt"
      threshold = if isDemo then 50 else 100
  input <- readFile fileIn
  let grid = fromString input 
      start = fst . fromJust . uncons $ findAll grid 'S' 
      end = fst . fromJust . uncons $ findAll grid 'E' 
      (steps,totalCost,costs) = fromJust $ path grid start end
  faster <- forM steps (\s -> do
    let cheatSteps = cheats 20 grid s
        faster = filter (\(_,_,val) -> val >= threshold) . map (\c -> (s,c,cheatVal costs s c)) $ cheatSteps
    return faster
    )
  let allFaster = concat faster
  putStrLn $ show (allFaster)

pt1 :: String -> Int
pt1 input = countCheats 2 100 grid start end
  where grid = fromString input 
        start = fst . fromJust . uncons $ findAll grid 'S' 
        end = fst . fromJust . uncons $ findAll grid 'E' 

pt2 :: String -> Int
pt2 input = countCheats 20 100 grid start end
  where grid = fromString input 
        start = fst . fromJust . uncons $ findAll grid 'S' 
        end = fst . fromJust . uncons $ findAll grid 'E' 

countCheats :: Int -> Int -> Grid Char -> Coord -> Coord -> Int
countCheats lim thresh g start end = length faster
  where (steps,_,costs) = fromJust $ path g start end
        faster = concatMap (\s -> filter ((>=thresh) . cheatVal costs s) $ cheats lim g s) steps

cheats :: Int -> Grid Char -> Coord -> [Coord]
cheats lim g (x,y) = filter (\c -> inBounds g c && findAt g c /= '#') $ opts
  where opts = [(x+vx,y+vy) | vx <- [-lim..lim], vy <- [-lim..lim], (abs vx) + (abs vy) <= abs lim]

-- yes this is duplicated from estCost
cheatCost :: Coord -> Coord -> Int
cheatCost (x,y) (x',y') = (abs (y'-y)) + (abs (x'-x))

cheatVal :: M.Map Coord Int -> Coord -> Coord -> Int
cheatVal costs s e = (M.findWithDefault 0 e costs) - ((M.findWithDefault 0 s costs) + (cheatCost s e))

------- TODO: Extract pathfinding to module

path :: Grid Char -> Coord -> Coord -> Maybe ([Coord],Int,M.Map Coord Int)
path g start end = findPath g end opens costs parents
  where
    opens = PQ.singleton (0,start)
    costs = M.singleton start 0
    parents = M.empty

findPath 
  :: Grid Char 
  -> Coord 
  -> PQ.MinQueue (Int,Coord) 
  -> M.Map Coord Int 
  -> M.Map Coord Coord 
  -> Maybe ([Coord],Int,M.Map Coord Int)
findPath _ _ PQ.Empty _ _ = Nothing
findPath g end opens costs parents = if current == end 
                                                then Just (
                                                  traceParents parents current, 
                                                  fromJust (M.lookup current costs),
                                                  costs
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
