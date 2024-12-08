module Day06 (pt1, pt2) where

import Data.List (nub)
import Data.Maybe

data Dir = N | E | S | W deriving (Show,Eq)

type Map = [[Char]]
type Coord = (Int,Int)
type Pos = (Dir,Coord)
type State = (Map, [Coord], Pos)

pt1 :: String -> Int
pt1 str = length $ mapFind 'X' m
  where ((m,_,_),_) = fromJust $ doStep [] (parse str)

-- bleh
mapFind :: Char -> Map -> [Coord]
mapFind c m = [ (x,y) | x <- [0..(length (head m))-1], y <- [0..(length m)-1], (mapGet (x,y) m) == c]

-- yeah I know, dont @ me
parse :: String -> State
parse str = (m, cs, (N,p))
  where m = lines str
        cs = mapFind '#' m
        p = head (mapFind '^' m) 


doStep :: [Pos] -> State -> Maybe (State,[Pos])
doStep ps s@(m, obs, pos@(d,p)) 
  | pos `elem` ps = Nothing
  | otherwise = if onMap p m 
                   then doStep (pos:ps) (m', obs, pos') 
                   else Just (s,ps) 
                     where pos' = pStep obs (d,p) 
                           m' = mapSet p 'X' m

pStep :: [Coord] -> Pos -> Pos
pStep obs pos
  | facingOb obs pos = turn pos
  | otherwise = next pos

facingOb :: [Coord] -> Pos -> Bool
facingOb obs pos = elem (snd (next pos)) obs

turn :: Pos -> Pos
turn (d,pos) = (case d of
                  N -> E
                  E -> S
                  S -> W
                  W -> N, pos)

next :: Pos -> Pos
next (d, (x,y)) = (d, case d of
                      N -> (x,y-1)
                      E -> (x+1,y)
                      S -> (x,y+1)
                      W -> (x-1,y))

prev :: Pos -> Pos
prev (d, (x,y)) = (d, case d of
                      N -> (x,y+1)
                      E -> (x-1,y)
                      S -> (x,y-1)
                      W -> (x+1,y))

onMap :: Coord -> Map -> Bool
onMap (x,y) m = 0 <= y && y < (length m) && 0 <= x && x < (length (head m))

mapGet :: Coord -> Map -> Char
mapGet (x,y) m = case drop x row of 
                   [] -> '!' 
                   (c:_) -> c
  where row = head $ drop y m

mapSet :: Coord -> Char -> Map -> Map
mapSet (x,y) c m = rsPre ++ ((rPre ++ (c:rPost)):(rsPost))
  where (rsPre, (row:rsPost)) = splitAt y m
        (rPre, (_:rPost)) = splitAt x row


pt2 :: String -> Int
pt2 str = length loopSpaces
  where 
    s@(m,obs,_) = parse str
    (_,(_:ps)) = fromJust $ doStep [] s -- drop last step out of bounds
    ps' = filter (not.facingOb obs) ps
    candidates = map next ps' -- obstacles are placed in front of the guard
    -- candidates are backwards, so remove obstacles that would have been placed earlier
    candidates' = removeSeenLater candidates 
    loopSpaces = nub . map snd . filter (loops m obs) $ candidates'

loops :: Map -> [Coord] -> Pos -> Bool
loops m obs (d,c) = case doStep [] (mapSet c 'O' m,(c:obs),prev (d,c)) of 
                      Nothing -> True 
                      Just _ -> False

removeSeenLater :: [Pos] -> [Pos]
removeSeenLater [] = []
removeSeenLater ((d,c):ps)
  | c `elem` (map snd ps) = removeSeenLater ps
  | otherwise = (d,c):(removeSeenLater ps)

