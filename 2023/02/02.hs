import System.IO
import Data.Char
import Data.Maybe
import qualified Data.Text as T

data Col = Blue | Green | Red deriving (Eq,Show,Read)
type Grp = (Col,Int)
type Set = [Grp]
type Game = (Int,[Set])

-- Boilerplate
main :: IO ()
main = do
  run "input-full.txt" day2

demo :: IO ()
demo = do
  run "input-demo.txt" day2

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

-- Const
maximums :: [Grp]
maximums = [(Red,12),(Green,13),(Blue,14)]

-- Main
day2 = pt2

pt1 = sum . map fst . filter possible . map parseLine . zip [1..]

pt2 = sum . map power . map parseLine . zip [1..]

possible :: Game -> Bool
possible = all possibleSet . snd

possibleSet :: Set -> Bool
possibleSet = all possibleGroup

possibleGroup :: Grp -> Bool
possibleGroup (col,num) = num <= max
  where max = fromJust $ lookup col maximums

power :: Game -> Int
power (_,sets) = product . map snd $ maxs
  where grps = concat sets
        maxs = foldr (\g acc -> maxGrp acc g) [] grps

maxGrp :: [Grp] -> Grp -> [Grp]
maxGrp maxs (col,n) = update col n' maxs
  where n' = max oldMax n
        oldMax = fromMaybe 0 $ lookup col maxs

-- Parsing Functions
parseLine :: (Int, String) -> Game
parseLine (i,str) = (i,game)
  where game = map parseSet sets
        sets = splitOn "; " setsStr
        setsStr = dropWord (dropWord str)

parseSet :: String -> Set
parseSet str = map parseGrp groups
  where groups = splitOn ", " str


parseGrp :: String -> Grp
parseGrp str = (col,count)
  where col = read (capitalise colStr)::Col
        count = read countStr::Int 
        (countStr:colStr:_) = words str

-- Helper functions
capitalise :: String -> String
capitalise (x:xs) = (toUpper x):xs
capitalise [] = []

dropWord :: String -> String
dropWord = tail . dropWhile (/= ' ') 

splitOn :: String -> String -> [String]
splitOn s input = map T.unpack (T.splitOn (T.pack s) ( T.pack input))

update :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
update k v assoc = (k,v):(filter ((k /=).fst) assoc)

