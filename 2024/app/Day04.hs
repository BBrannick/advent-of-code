module Day04 (pt1, pt2) where

import Data.List
import Text.Regex

pt1 :: String -> Int
pt1 str = sum $ map (occs xmasRegex) [hor, ver, diag, negDiag]
  where hor = lines str
        ver = transpose hor
        diag = diagonals (reverse hor)
        negDiag = diagonals hor

occs :: Regex -> [String] -> Int
occs rx strs = sum $ map (countMatches rx) strs

pt2 :: String -> Int
pt2 = length

countMatches :: Regex -> String -> Int
countMatches rx str = case matchRegexAll rx str of
                        Nothing -> 0
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
