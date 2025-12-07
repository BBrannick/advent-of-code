module Day07 (pt1, pt2) where

import Data.List
import Data.Maybe

pt1 :: String -> Int
pt1 ls = fst . foldl' run (0,[s]) $ rows
  where (start,rows) = fromJust . uncons $ lines ls
        s = fromJust . elemIndex 'S' $ start


pt2 :: String -> Int
pt2 ls = sum . foldl' run2 initial $ rows
  where (start,rows) = fromJust . uncons $ lines ls
        initial = map (\c -> if c == 'S' then 1 else 0) start

run :: (Int,[Int]) -> String -> (Int,[Int])
run (f,xs) str = (f + length found, new)
  where found = intersect xs $ elemIndices '^' str
        new = (xs \\ found) `union` (concatMap (\x -> [x-1,x+1]) found)

run2 :: [Int] -> String -> [Int]
run2 xs str = map process ws
  where ws = window3pad (0,'.') (zip xs str)

process :: ((Int,Char),(Int,Char),(Int,Char)) -> Int
process (_,(_,'^'),_) = 0
process ((l,lc),(m,_),(r,rc)) = m + (if lc=='^' then l else 0) + (if rc=='^' then r else 0)

window3pad :: a -> [a] -> [(a,a,a)]
window3pad p (x:y:rest) = (p,x,y):window3pad' p (x:y:rest)
window3pad p [x] = [(p,x,p)]
window3pad _ _ = []

window3pad' :: a -> [a] -> [(a,a,a)]
window3pad' p (x:y:z:rest) = (x,y,z):window3pad' p (y:z:rest)
window3pad' p (x:y:[]) = [(x,y,p)]
window3pad' _ _ = []
