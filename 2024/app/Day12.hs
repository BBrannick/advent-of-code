module Day12 (pt1, pt2) where

import Grid
import qualified Data.Map as M
import Debug.Trace

pt1 :: String -> Int
pt1 s = M.foldr (\(a,p) acc -> acc + (a*p)) 0 $ foldr (\c m -> updateMap regions m c) M.empty coords
  where grid = fromString s
        (mx,my) = bounds grid
        coords = [(x,y) | y <- [0..my], x <- [0..mx]]
        regions = labelGrid grid
  {-
pt1 s = sum.map snd.snd $ foldr (\(x,y) (g,rs) -> let (g',r) = region' g (x,y) in (g',r:rs)) (grid,[]) coords
-}

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

{-
region' :: Grid Char -> Coord -> (Grid Char,(Int,Int))
region' g (x,y) = plot (setCoord (x,y) '!' g) c (x,y)
  where c = findAt g (x,y)

plot :: Grid Char -> Char -> Coord -> (Grid Char,(Int,Int))
plot g c (x,y) = traceShowId $ (setCoord (x,y) '.' g'', (1+area, (4-(length sibs)) + perim))
  where adjs = filter (inBounds g) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
        sibs = filter ((\a -> a == c || a == '!') . findAt g) adjs
        nexts = filter ((==c) . findAt g) sibs
        gWithSeen = foldr (\co grid -> setCoord co '!' grid) g nexts
        (g'',(area,perim)) = foldr (\co (gr,(a,p)) -> let (gr',(a',p')) = plot gr c co in (gr',(a+a',p+p'))) (gWithSeen,(0,0)) nexts

-}


pt2 :: String -> Int
pt2 = length


