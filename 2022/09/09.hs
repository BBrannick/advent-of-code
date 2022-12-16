import System.IO
import Data.List

main :: IO ()
main = do
  run "input.txt" day9

demo = do
  run "input-demo-2.txt" day9

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

data Dir = U | R | D | L deriving (Show,Read)
type Move = (Dir,Int)

type Pos = (Int,Int)

parseDir :: String -> Move
parseDir s = (read d::Dir, read n::Int)
  where [d,n] = words s

day9 = pt2

pt1 = length . nub . snd . runMoves 1 . map parseDir
pt2 = length . nub . snd . runMoves 9 . map parseDir

vec :: Move -> (Int,Int)
vec (U,n) = (0,n)
vec (R,n) = (n,0)
vec (D,n) = (0,-n)
vec (L,n) = (-n,0)

move :: Move -> Pos -> Pos
move m (x,y) = (x+dx, y+dy)
  where (dx, dy) = vec m

runMoves :: Int -> [Move] -> ((Pos,[Pos]),[Pos])
runMoves n = foldl
  (\((h,ks),ts) move -> let ((h',ks'),ts') = mkMove move (h,ks) in ((h',ks'),ts++ts'))
  (((0,0),(replicate n (0,0))), [])

mkMove :: Move -> (Pos,[Pos]) -> ((Pos,[Pos]),[Pos])
mkMove (_,0) r@(_,t:_) = (r,[t])
mkMove (d,n) (h,ks) = (r', (head ks'):ts)
  where h' = move (d,1) h
        ks' = foo ks h'
        (r',ts) = mkMove (d,n-1) (h',ks')

follow :: Pos -> Pos -> Pos
follow (tx,ty) (hx,hy)
  | abs dx <= 1 && abs dy <= 1 = (tx,ty)
  | otherwise = (tx + dx', ty + dy')
    where dx = hx - tx
          dy = hy - ty
          dx' = signum dx
          dy' = signum dy

foo :: [Pos] -> Pos -> [Pos]
foo ks h = snd $ foldr (\k (prev,ks') -> let k' = follow k prev in (k',k':ks')) (h,[]) ks


