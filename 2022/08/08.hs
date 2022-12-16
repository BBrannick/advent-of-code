import System.IO
import Data.Char
import Data.List

main :: IO ()
main = do
  run "input.txt" day8

demo = do
  run "input-demo.txt" day8

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

day8 = pt2

pt1 ls = length ( filter (\(x,y) -> seen y x g g') ps )
  where g = parse ls
        g' = transpose g
        maxX = (length (head g)) - 1
        maxY = (length g) - 1
        ps = pairs maxX maxY

pt2 ls = maximum (map (score g g') ps)
  where g = parse ls
        g' = transpose g
        maxX = (length (head g)) - 1
        maxY = (length g) - 1
        ps = pairs maxX maxY

pairs :: Int -> Int -> [(Int,Int)]
pairs mx my = [ (x,y) | x<-[0..mx], y<-[0..my] ]

pairs' :: Int -> Int -> [(Int,Int)]
pairs' mx my = map (map (,) [0..mx]) [0..my]

score :: [[Int]] -> [[Int]] -> (Int,Int) -> Int
score m m' (y,x) = foldl1 (*) (map (numSeen h) ls)
  where (h,ls) = pluck y x m m'

numSeen :: Int -> [Int] -> Int
numSeen _ [] = 0
numSeen h (x:xs) 
  | x < h = 1 + numSeen h xs
  | otherwise = 0

parse :: [String] -> [[Int]]
parse = map (map digitToInt)

seen :: Int -> Int -> [[Int]] -> [[Int]] -> Bool
seen y x m m' = seen' h ls
  where (h, ls) = pluck y x m m'

pluck :: Int -> Int -> [[Int]] -> [[Int]] -> (Int, [[Int]])
pluck y x m m' = (h, [reverse left,right,reverse top,bottom])
  where row = m!!y
        col = m'!!x
        (left,(h:right)) = splitAt x row
        (top,(_:bottom)) = splitAt y col

seen' :: Int -> [[Int]] -> Bool
seen' h lines = any (all (< h)) lines

