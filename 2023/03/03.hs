import System.IO
import qualified Debug.Trace as Db
import Data.Char

debugging = False

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
main' = pt1

pt1 :: [String] -> Int
pt1 lines = windowFoldl processLine 0 (trace ("lines: " ++ show padLines) padLines)
  where padLines = []:lines++[[]]

processLine total (prev, curr, next) = total + processLine' prev next curr

processLine' prev next [] = 0
processLine' prev next xs = trace ("found result: " ++ show result) $ 
  result + processLine' (drop dropped prev) (drop dropped next) remaining
  where (result,remaining) = processNextNumber (prev,xs,next)
        dropped = traceShowId $ length xs - length remaining

processNextNumber arg@(prevLine, str, nextLine) = trace ("processNextNumber: " ++ show arg) (result, rest')
  where (dropped,rest) = span (not.isDigit) str
        (numS,rest') = span isDigit rest
        numWidth = length numS
        numValue = if null numS then 0 else read numS::Int
        (start,ln) = (length dropped - 1, numWidth + 2)
        prevStrSymbol = symbolInLine prevLine start ln
        nextStrSymbol = symbolInLine nextLine start ln
        thisLineSymbol = symbolInLine str start ln
        result = if or [prevStrSymbol,nextStrSymbol,thisLineSymbol]
                    then numValue
                    else 0

symbolInLine :: String -> Int -> Int -> Bool
symbolInLine line start len = any isPartSymbol . dropTake start len $ line

isPartSymbol :: Char -> Bool
isPartSymbol c = c /= '.' && not (isDigit c)

dropTake :: Int -> Int -> [a] -> [a]
dropTake d t = take t . drop (max 0 d)

windows3 :: [a] -> [(a,a,a)]
windows3 xs = zip3 xs (drop 1 xs) (drop 2 xs)

windowFoldl :: (b -> (a,a,a) -> b) -> b -> [a] -> b
windowFoldl fn acc ls = foldl fn acc $ windows3 ls

-------

trace msg val = trace' debugging msg val
trace' True msg val = Db.trace msg val
trace' False _ val = val

traceShowId = tSI debugging
tSI True = Db.traceShowId
tSI False = id



