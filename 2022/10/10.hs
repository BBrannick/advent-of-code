import System.IO
import Debug.Trace

main :: IO ()
main = do
  run "input.txt" day10

demo = do
  run "input-demo.txt" day10

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

day10 = pt2

pt1 = sum . map str . findCycles cycles . parse
pt2 = chunk 40 . draw . parse

cycles = [20,60..220]

str :: (Int,Int) -> Int
str = uncurry (*)

findCycles :: [Int] -> [(Int,Int)] -> [(Int,Int)]
findCycles cs totals = map (\c -> (c, snd(takeLast ((<c).fst) totals))) cs

parse :: [String] -> [(Int,Int)]
parse = scanl (\(t,n) (t',n') -> (t+t',n+n')) (0,1) . map parseIns

parseIns :: String -> (Int,Int)
parseIns s
  | s == "noop" = (1,0)
  | otherwise = (2,read (drop 5 s)::Int)

takeLast :: (a -> Bool) -> [a] -> a
takeLast f = head . reverse . takeWhile f

draw :: [(Int,Int)] -> String
draw = snd . foldl drawIns ((0,1),"")

drawIns :: ((Int,Int),String) -> (Int,Int) -> ((Int,Int),String)
drawIns ((t,n),s) (t',n') = 
  (b, s ++ map (\cycle -> if abs (n-(cycle `mod` 40)) < 2 then '#' else '.') [t..(t'-1)])

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = x':(chunk n xs')
  where (x', xs') = splitAt n xs
