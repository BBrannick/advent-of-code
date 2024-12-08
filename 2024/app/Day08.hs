module Day08 (pt1, pt2) where

import Data.Char
import Data.List
import qualified Data.Map as Map

type Co = (Int,Int)
type Nodes = Map.Map Char [Co]

pt1 :: String -> Int
pt1 str = length . filter (inboundsStr str) . nub . concatMap allAntinodes . Map.elems . parse $ str

pt2 :: String -> Int
pt2 str = length . nub . concatMap (allAntinodes' (bounds str)) . Map.elems . parse $ str

---

parse :: String -> Nodes
parse str = foldr (\y m -> parse' y m (ls!!y)) Map.empty [0..(length ls)-1]
  where ls = lines str

parse' :: Int -> Nodes -> String -> Nodes
parse' y ns str = foldr (\x m -> addNode m x y (str!!x)) ns [0..(length str)-1]

addNode :: Nodes -> Int -> Int -> Char -> Nodes
addNode ns x y c
  | isNode c = Map.insertWith (++) c [(x,y)] ns
  | otherwise = ns

isNode :: Char -> Bool
isNode c = isLetter c || isNumber c

simplify :: Int -> Int -> (Int,Int)
simplify x y
  | d == 1 = (x,y)
  | otherwise = simplify (x `div` d) (y `div` d)
  where d = gcd x y

---

allAntinodes :: [Co] -> [Co]
allAntinodes = concatMap (uncurry antinodes) . pairs

allAntinodes' :: Co -> [Co] -> [Co]
allAntinodes' c = concatMap (uncurry (antinodes' c)) . pairs

antinodes :: Co -> Co -> [Co]
antinodes (x,y) (x',y') = [(x'+vx,y'+vy),(x-vx,y-vy)]
  where (vx,vy) = (x'-x,y'-y)

antinodes' :: Co -> Co -> Co -> [Co]
antinodes' b (x,y) (x',y') = lower ++ upper
  where v = (x'-x,y'-y)
        (vx,vy) = uncurry simplify v
        lower = draw b (x,y) (-vx,-vy)
        upper = draw b (x',y') (vx,vy)

draw :: Co -> Co -> Co -> [Co]
draw b (x,y) (vx,vy) 
  | inbounds b (x,y) = (x,y):(draw b (x+vx,y+vy) (vx,vy))
  | otherwise = []

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = (zip (repeat x) xs) ++ pairs xs

inboundsStr :: String -> Co -> Bool
inboundsStr str = inbounds (bounds str)

inbounds :: Co -> Co -> Bool
inbounds (mx,my) (x,y) = 0 <= y && y <= my && 0 <= x && x <= mx

bounds :: String -> Co
bounds str = (x,y)
  where ls = lines str
        y = length ls - 1
        x = length (head ls) - 1

---


