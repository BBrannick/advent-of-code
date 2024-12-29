module Day12 (pt1, pt2) where

import Grid
import qualified Data.Map as M

data Dir = N | E | S | W deriving (Show,Eq,Enum)

pt1 :: String -> Int
pt1 s = M.foldr (\(a,p) acc -> acc + (a*p)) 0 $ foldr (\c m -> updateMap regions m c) M.empty coords
  where grid = fromString s
        (mx,my) = bounds grid
        coords = [(x,y) | y <- [0..my], x <- [0..mx]]
        regions = labelGrid grid

updateMap :: Grid Int -> M.Map Int (Int,Int) -> Coord -> M.Map Int (Int,Int)
updateMap g m c@(x,y) = M.insertWith (\(a,p) (a',p') -> (a+a',p+p')) r (area,perim) m
  where area = 1
        r = findAt g c
        perim = length $ filter (\c' -> not(inBounds g c') || findAt g c' /= r) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]


labelGrid :: Grid Char -> Grid Int
labelGrid g = thrd $ foldr (\c (i,gc, gi) -> region i gc gi c) (1,g,initGrid) coords
  where 
    (mx,my) = bounds g
    initGrid = replicate (my+1) (replicate (mx+1) 0)
    coords = [(x,y) | y <- [0..my], x <- [0..mx]]
    thrd = (\(_,_,x) -> x)

region :: Int -> Grid Char -> Grid Int -> Coord -> (Int,Grid Char,Grid Int)
region i gc gi c 
  | findAt gc c == '!' = (i, gc, gi)
  | otherwise = (i+1, gc', gi')
    where (gc', gi') = mapRegion i gc gi c

mapRegion :: Int -> Grid Char -> Grid Int -> Coord -> (Grid Char, Grid Int)
mapRegion i gc gi co = mapRegion' i gc' gi' ch co
  where ch = findAt gc co 
        gc' = setCoord co '!' gc
        gi' = setCoord co i gi

mapRegion' :: Int -> Grid Char -> Grid Int -> Char -> Coord -> (Grid Char, Grid Int) -- good god
mapRegion' i gc gi ch (x,y) = (gc'', gi'')
  where adjs = filter (inBounds gc) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        nexts = filter ((==ch) . findAt gc) adjs
        gc' = foldr (\co grid -> setCoord co '!' grid) gc nexts
        gi' = foldr (\co grid -> setCoord co i grid) gi nexts
        (gc'',gi'') = foldr (\co (fgc, fgi) -> mapRegion' i fgc fgi ch co) (gc',gi') nexts

-- it got worse
pt2 :: String -> Int
pt2 input = fst $ foldl' (\(acc,g) c -> let (x,g') = processRegion g c in (acc+x,g')) (0,grid) coords
  where
    grid = fromString input
    (mx,my) = bounds grid
    coords = [(x,y) | y <- [0..my], x <- [0..mx]]

processRegion :: Grid Char -> Coord -> (Int,Grid Char)
processRegion g c 
  | findAt g c == '.' = (0,g)
  | otherwise = (a * (s+inners),g')
  where coords = regionCoords g c
        a = length coords
        (s,seen) = sides g c N
        inners = innerSides g (findAt g c) coords seen
        g' = setCoords g $ zip coords (repeat '.')

regionCoords :: Eq a => Grid a -> Coord -> [Coord]
regionCoords g c = rcs g [c] [c]

rcs :: Eq a => Grid a -> [Coord] -> [Coord] -> [Coord]
rcs _ [] seen = seen
rcs g ((x,y):candidates) seen = rcs g (candidates++news) (seen++news)
  where regionId = findAt g (x,y)
        adjs = filter ((==regionId) . findAt g) . filter (inBounds g) $ [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        news = filter (not.(`elem` seen)) adjs


turnR :: Dir -> Dir
turnR W = N
turnR d = succ d

turnL :: Dir -> Dir
turnL N = W
turnL d = pred d

sides :: Eq a => Grid a -> Coord -> Dir -> (Int,[(Coord,Dir)])
sides g c d = sides' g (findAt g c) c [] d 0

sides' :: Eq a => Grid a -> a -> Coord -> [(Coord,Dir)] -> Dir -> Int -> (Int,[(Coord,Dir)])
sides' g x c seen dir acc
  | (c,dir) `elem` seen = (acc,seen)
  | (inBounds g leftC) && (left == x) = sides' g x leftC ((c,dir):seen) leftD (acc+1)
  | (not (inBounds g frontC)) || (front /= x) = sides' g x c ((c,dir):seen) (turnR dir) (acc+1)
  | otherwise = sides' g x frontC ((c,dir):seen) dir acc
  where 
    frontC = next dir c
    front = findAt g frontC
    (leftC,leftD) = followL dir c
    left = findAt g leftC

innerSides :: Eq a => Grid a -> a -> [Coord] -> [(Coord,Dir)] -> Int
innerSides g x coords seen = fst $ foldr (\(c,d) (acc,seen') -> if (c,d) `elem` seen' 
                                                                   then (acc,seen') 
                                                                   else let (ss,seen'') = sides g c d 
                                                                         in (acc + ss, seen'' ++ seen')
                                                                        ) (0,[]) candidates
  where candidates = filter (not.(`elem` seen)) . filter ((/=x) . findAt g . adjacent ) . filter (inBounds g . adjacent) . concatMap edges $ coords
        edges = (\c -> zip (repeat c) [N,E,S,W,E])
        adjacent = (\(c,d) -> next (turnL d) c)


next :: Dir -> Coord -> Coord
next d (x,y) = case d of
                 N -> (x,y-1)
                 E -> (x+1,y)
                 S -> (x,y+1)
                 W -> (x-1,y)

followL :: Dir -> Coord -> (Coord,Dir)
followL d c = let d' = turnL d in (next d' c, d')

