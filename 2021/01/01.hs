import System.IO

main :: IO ()
main = do
	ls <- getReadings
	print . countIncs $ ls
	print . countWindowIncs' 3 $ ls

getReadings :: IO ([Int])
getReadings = do
	handle <- openFile "input.txt" ReadMode
	contents <- hGetContents handle
	return $ map (read::String->Int) . lines $ contents
	
countIncs :: Ord a => [a] -> Int
countIncs = countWindowIncs' 1

countWindowIncs' :: Ord a => Int -> [a] -> Int
countWindowIncs' n xs = 
	length . filter (\(x,y) -> y > x) $ (zip xs (drop n xs))
