module Day02 (pt1, pt2) where

import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map


-- Boilerplate
main :: IO ()
main = do
  run "full.txt" main'

demo :: IO ()
demo = do
  run "demo.txt" main'

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents


-- Main
main' = pt2

pt1 :: [String] -> Int
pt1 = length . filter isSafe . map parse 

parse :: String -> [Int]
parse = map (read::String->Int) . words 

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe (x:[]) = True
isSafe report@(x:y:rest)
  | x < y = isSafe' safeInc report
  | x > y = isSafe' safeDec report
  | otherwise = False

isSafe' _ [] = True
isSafe' _ (x:[]) = True
isSafe' cond (x:y:rest) = (cond x y) && isSafe' cond (y:rest)

safeInc :: Int -> Int -> Bool
safeInc x y = x < y && y <= (x+3)

safeDec :: Int -> Int -> Bool
safeDec x y = x > y && y >= (x-3)

-- Pt 2
pt2 :: [String] -> Int
pt2 = length . filter isSafeish . map parse


isSafeish :: [Int] -> Bool
isSafeish reports = isSafe reports || any isSafe (sublists reports)

sublists :: [Int] -> [[Int]]
sublists xs = sublists' (length xs) xs

sublists' :: Int -> [Int] -> [[Int]]
sublists' 0 xs = []
sublists' n xs = ((take (n-1) xs) ++ (drop n xs)):(sublists' (n-1) xs)
