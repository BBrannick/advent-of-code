import System.IO
import Data.List

main :: IO ()
main = do
  run "input.txt" day6

demo = do
  run "input-demo.txt" day6

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

day6 :: [String] -> [Int]
day6 = map pt2

pt1 = uniqSubstrIdx 4
pt2 = uniqSubstrIdx 14

uniqSubstrIdx :: Int -> String -> Int
uniqSubstrIdx n xs = (length xs) - (usi n n xs)

usi :: Int -> Int -> String -> Int
usi _ 0 xs = length xs
usi _ _ [] = 0
usi n n' (x:xs) = case x `elemIndex` (take (n' - 1) xs) of
                    Just i -> usi n n xs
                    Nothing -> usi n (n'-1) xs
