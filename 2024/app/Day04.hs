module Day04 (pt1, pt2) where

import Data.List
import Text.Regex

pt1 :: String -> Int
pt1 str = sum $ map (occs xmasRegex) [rows, cols, diag, negDiag]
  where rows = lines str
        cols = transpose rows
        diag = diagonals (reverse rows)
        negDiag = diagonals rows

occs :: Regex -> [String] -> Int
occs rx = sum . map (countMatches rx) 

countMatches :: Regex -> String -> Int
countMatches rx str = case matchRegexAll rx str of
                        Just (_,(_:mas),rest,_) -> 1 + countMatches rx (mas++rest)
                        _ -> 0

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals matrix = map (diagonals' matrix) [0..maxX]
  where maxX = (length (head matrix) - 1) + (length matrix - 1)

diagonals' :: [[a]] -> Int -> [a]
diagonals' _ (-1) = []
diagonals' [] _ = []
diagonals' (l:ls) n 
  | n < (length l) = (l!!n):(diagonals' ls (n-1)) 
  | otherwise = diagonals' ls (n-1)

xmasRegex :: Regex
xmasRegex = mkRegex "(XMAS|SAMX)"

---
---

pt2 :: String -> Int
pt2 = sum . window3Map countInRow . lines

countInRow :: (String,String,String) -> Int
countInRow (top@('M':_:'S':_), mid@(_:'A':_), bot@('M':_:'S':_)) = 1 + countInRow (tail top,tail mid, tail bot)
countInRow (top@('M':_:'M':_), mid@(_:'A':_), bot@('S':_:'S':_)) = 1 + countInRow (tail top,tail mid, tail bot)
countInRow (top@('S':_:'S':_), mid@(_:'A':_), bot@('M':_:'M':_)) = 1 + countInRow (tail top,tail mid, tail bot)
countInRow (top@('S':_:'M':_), mid@(_:'A':_), bot@('S':_:'M':_)) = 1 + countInRow (tail top,tail mid, tail bot)
countInRow (_:top,_:mid,_:bot) = countInRow (top,mid,bot)
countInRow _ = 0

window3Map :: ((a,a,a) -> b) -> [a] -> [b]
window3Map _ [] = []
window3Map _ [_] = []
window3Map _ [_,_] = []
window3Map f (x:y:z:rest) = (f (x,y,z)):(window3Map f (y:z:rest))

