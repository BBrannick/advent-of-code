module Main where
	
import Day02 (pt1, pt2)

main :: IO ()
main = do
	demoData <- readFile "data/02_demo.txt"
	realData <- readFile "data/02.txt"
	print $ pt1 demoData
	print $ pt1 realData
	print $ pt2 demoData
	print $ pt2 realData

