import System.IO
import Data.List

-- (Move,From,To)
-- (1,2,3) <- move 1 from 2 to 3
type Inst = (Int,Int,Int)

--     [D]    
-- [N] [C]    
-- [Z] [M] [P]
--  1   2   3 
--
--  [['N','Z'],['D','C','M'],['P']]
type Stacks = [[Char]]

main :: IO ()
main = do
	run "input.txt" day5

demo = do
	run "input-demo.txt" day5

run filename fn = do
	ls <- getLines filename
	print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
	handle <- openFile filename ReadMode
	contents <- hGetContents handle
	return $ lines contents

day5 = length

parseInput :: [String] -> (Stacks, [Inst])
parseInput ls = (parseStack stackLs, map parseInst instLs)
  where stackLs = takeWhile (not.null) ls
        instLs = drop (1 + length stackLs) ls

parseStack :: [String] -> Stacks
parseStack = map (parseStack'.concat) . transpose . map (chunk 4)

parseStack' :: String -> [Char]
parseStack' s = map (head . drop 1) (init (words s))

parseInst :: String -> Inst
parseInst is = (x,y,z)
  where (_:a:_:b:_:c:_) = words is 
        (x:y:z:_) = map (read::String->Int) [a,b,c]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = let (c,rest) = splitAt n l in c:(chunk n rest)
