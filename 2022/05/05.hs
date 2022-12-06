import System.IO
import Data.List
import Control.Monad

type State = (Stacks,[Inst])

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

day5 = pt2

pt1 :: [String] -> String
pt1 = map head . filter (not.null) . doInstr . parseInput

pt2 :: [String] -> String
pt2 = map head.  filter (not.null) . doInstr' . parseInput

pt1' :: [String] -> IO (String)
pt1' ls = do
  let (sts,is) = parseInput ls
  res <- foldM (\stk i -> do
    prntStk stk
    prntIns i
    return $ do' i stk) sts is
  return $ map head . filter (not.null) $ res

prntStk stk = do
  mapM_ (print.reverse) stk

prntIns (x,y,z) = do
  putStrLn $ "move "++(show x)++" from "++(show y)++" to "++(show z)

doInstr :: State -> Stacks
doInstr (st,is) = foldl (flip do') st is

doInstr' :: State -> Stacks
doInstr' (st,is) = foldl (flip do'') st is

do' :: Inst -> Stacks -> Stacks
do' (x,y,z) sts = stkPush z d sts'
  where (d,sts') = stkPop y x sts

do'' :: Inst -> Stacks -> Stacks
do'' (x,y,z) sts = stkPush' z d sts'
  where (d,sts') = stkPop y x sts

stkPop :: Int -> Int -> Stacks -> ([Char],Stacks)
stkPop i n sts = (d,pre++[s]++post)
  where d = pop n (sts!!(i-1))
        s = drop n (sts!!(i-1))
        pre = take (i-1) sts
        post = drop i sts

stkPush :: Int -> [Char] -> Stacks -> Stacks
stkPush i d sts = pre++[s]++post
  where s = push d (sts!!(i-1))
        pre = take (i-1) sts
        post = drop i sts

stkPush' :: Int -> [Char] -> Stacks -> Stacks
stkPush' i d sts = pre++[s]++post
  where s = d++(sts!!(i-1))
        pre = take (i-1) sts
        post = drop i sts

push :: String -> String -> String
push [] s = s
push (x:xs) s = push xs (x:s)

pop :: Int -> String -> String
pop 0 _ = ""
pop _ [] = error "cannot pop from empty list"
pop n (x:xs) = x:(pop (n-1) xs)

parseInput :: [String] -> State
parseInput ls = (parseStack stackLs, map parseInst instLs)
  where stackLs = takeWhile (not.null) ls
        instLs = drop (1 + length stackLs) ls

parseStack :: [String] -> Stacks
parseStack = map parseStack' . transpose . map (chunk 4)

parseStack' :: [String] -> [Char]
parseStack' = dropWhile (==' ') . map (head . drop 1) . init

parseInst :: String -> Inst
parseInst is = (x,y,z)
  where (_:a:_:b:_:c:_) = words is 
        (x:y:z:_) = map (read::String->Int) [a,b,c]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = let (c,rest) = splitAt n l in c:(chunk n rest)
