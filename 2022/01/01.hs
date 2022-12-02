import System.IO
import Data.List

main :: IO ()
main = do
	contents <- loadFile
	let groups = splitOn "" $ lines contents
	    most = nGrtLoad 3 $ map (parse) groups
	putStrLn . show $ sum most

loadFile = do
	handle <- openFile "input-full.txt" ReadMode
	hGetContents handle

parse :: [String] -> [Int]
parse = map (read::String->Int)

grtLoad :: [[Int]] -> Int
grtLoad = maximum . (map sum)

nGrtLoad :: Int -> [[Int]] -> [Int]
nGrtLoad n grps = 
	foldl 
	  (\topN l -> take n . sortBy (flip compare) $ (l:topN)) 
		[] 
		loads
  where loads = map sum grps

splitOn :: Eq(a) => a -> [a] -> [[a]]
splitOn = splitOn' []

splitOn' :: Eq(a) => [a] -> a -> [a] -> [[a]]
splitOn' curr _ [] = [curr]
splitOn' curr n (x:xs) 
	| n == x    = curr:(splitOn' [] n xs)
  | otherwise = (splitOn' (x:curr) n xs)
