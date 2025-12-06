module Day06 (pt1, pt2) where

import Data.List
import Data.List.Split (splitWhen)
import Data.Char

pt1 :: String -> Int
pt1 = sum . map calc . transpose . map words . reverse . lines

pt2 :: String -> Int
pt2 = sum . map calcCeph . splitWhen (all isSpace) . transpose . lines

-- Expects each column of a calculation as strings
-- e.g. ["1  *","24  ","356 "]
calcCeph :: [String] -> Int
calcCeph xs@(x:_) = foldr1 f ints
  where f = readFn (last x)
        ints = map ((read::String->Int) . init) xs
calcCeph _ = error "Invalid input"

-- Expects each row of a calculation as strings, trimmed, with function in the head
-- e.g. ["*","6","45","123"]
calc :: [String] -> Int
calc (fs:args) = foldr1 f ints
  where f = readFn (head fs)
        ints = map (read::String->Int) args
calc _ = error "Invalid input"

readFn :: Char -> (Int -> Int -> Int)
readFn '+' = (+)
readFn '*' = (*)
readFn _ = error "Unrecognised function"
