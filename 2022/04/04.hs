import System.IO
-- import Data.List.Split

main :: IO ()
main = do
	run "input.txt" day4

demo = do
	run "input-demo.txt" day4

run filename fn = do
	ls <- getLines filename
	print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
	handle <- openFile filename ReadMode
	contents <- hGetContents handle
	return $ lines contents

day4 = pt2

pt1 :: [String] -> Int
pt1 = length . filter (contains . parse)

pt2 :: [String] -> Int
pt2 = length . filter (overlap . parse)

contains :: ((Int,Int),(Int,Int)) -> Bool
contains ((x,y),(x',y')) 
  | x == x' || y == y' = True
	| x < x'    = y >= y' 
  | otherwise = y <= y'

overlap :: ((Int,Int),(Int,Int)) -> Bool
overlap ((x,y),(x',y'))
	| x == x' || y == y' = True
	| x < x' = y >= x'
  | otherwise = x <= y'

parse :: String -> ((Int,Int),(Int,Int))
parse = pair . map (read::String->Int) . splitOneOf "-,"

pair [a,b,c,d] = ((a,b),(c,d))

splitOneOf delims xs = sOO [] [] delims xs

sOO :: Eq a => [[a]] -> [a] -> [a] -> [a] -> [[a]]
sOO splits curr _ [] = reverse (reverse curr:splits)
sOO splits curr delims (x:xs)
  | x `elem` delims = sOO (reverse curr:splits) [] delims xs
  | otherwise = sOO splits (x:curr) delims xs

