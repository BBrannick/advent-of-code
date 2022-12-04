import System.IO
import Data.Char
import Data.List

main = do
	run "input.txt"

demo = do
	run "input-demo.txt"

run filename = do
	ls <- getLines filename
	print $ day3 ls

getLines :: String -> IO ([String])
getLines filename = do
	handle <- openFile filename ReadMode
	contents <- hGetContents handle
	return $ lines contents

day3 :: [String] -> Int
day3 = pt1

pt1 = sum . map (prio.err)

pt2 = sum . map (prio.cmn) . (chunk 3)

cmn :: [String] -> Char
cmn = head . foldr1 intersect

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = let (c,rest) = splitAt n l in c:(chunk n rest)

prio :: Char -> Int
prio c 
  | c >= 'a' = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

err :: String -> Char
err = head . uncurry intersect . bisect

bisect s = splitAt (length s `div` 2) s
