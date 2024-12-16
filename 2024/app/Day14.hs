module Day14 (pt1, pt2) where

import Control.Monad
import Data.List (partition, sort)
import Data.Char
import qualified Data.Map as M
import Text.Regex
import Grid


--type Coord = (Int,Int)
type Bot = (Coord,Coord)


--- Helper to check what the grid looks like after a given number of steps
main :: IO ()
main = do
  input <- readFile "data/14.txt"
  let bots = map parse (lines input)
      bs = (101,103)
  forever $ do
    stepsStr <- getLine
    let steps = (read::String->Int) stepsStr
    printAfterSteps bots steps

printAfterSteps :: [Bot] -> Int -> IO ()
printAfterSteps bots n = do 
  let bots' = map (step n (101,103)) bots 
      g = divg . toGrid (101,103) $ bots'
  putStrLn (show n)
  draw g
  putStrLn ("===============")


--- Attempted this, but could never find a balance of candidates that really worked
-- Either tended to return far too many maps, or none
pt2IO :: IO ()
pt2IO = do
  input <- readFile "data/14.txt" 
  let bots = map parse (lines input) 
  forever $ do
    ins <- sequence [getLine, getLine]
    let [l,l'] = map (read::String->Int) ins
    findCandidates bots l l'
--- 
findCandidates :: [Bot] -> Int -> Int -> IO ()
findCandidates bots lines botsinline = do
  let lims = (101,103)
  forM [1..(101*103)] (\n -> do
    let bots' = map (step n lims) bots
    when (candidate bots' lines botsinline) $ do
      let g = divg . toGrid lims $ bots
      putStrLn (show n)
      draw g
      putStrLn "====================================="
             )
  print "done"

candidate :: [Bot] -> Int -> Int -> Bool
candidate bs n n' = (>=n) . length . filter (>=n') . M.elems . M.fromListWith (+) . flip zip (repeat 1) . map (snd.fst) $ bs


--- I gave up on naming conventions
foo :: IO ()
foo = do
  input <- readFile "data/14.txt"
  let s = pt2 input
      bots = map parse (lines input)
  forM s (printAfterSteps bots)
  print "done"

pt1 :: String -> Int
pt1 s = securityFactor bs . map (step 100 bs . parse) $ ls
  where ls = lines s
        bs = if length ls > 20 then (101,103) else (11,7)

-- Find the 50 lowest security factor (entropy) grids, and return the steps
pt2 :: String -> [Int]
pt2 s = take 50 . map snd . sort . take (101*103) . flip zip [0..] . map (securityFactor bs) . iterate (map (step 1 bs)) . map parse $ ls
  where ls = lines s
        bs = if length ls > 20 then (101,103) else (11,7)

securityFactor :: Coord -> [Bot] -> Int
securityFactor (mx,my) bs = product . map (length) $ [tl,bl,tr,br]
  where hx = mx `div` 2
        hy = my `div` 2
        bs' = filter (\((x,y),_) -> x /= hx && y /= hy) bs
        (ls,rs) = partition (\((x,_),_) -> x < hx) bs'
        (tl,bl) = partition tbPart ls
        (tr,br) = partition tbPart rs
        tbPart = (\((_,y),_) -> y < hy)

step :: Int -> Coord -> Bot -> Bot
step n (mx,my) ((x,y),(vx,vy)) = ((x',y'),(vx,vy))
  where x' = (x + (vx * n)) `mod` mx 
        y' = (y + (vy * n)) `mod` my

divg :: Grid Char -> Grid Char
divg g = foldr (\c g' -> setCoord c ' ' g') g $ [(x,hy) | x <- [0..mx]] ++ [(hx,y) | y <- [0..my]]
  where (mx,my) = bounds g
        hx = (mx+1) `div` 2
        hy = (my+1) `div` 2

toGrid :: Coord -> [Bot] -> Grid Char
toGrid (mx,my) bs = map (map (\c -> if c == 0 then '.' else intToDigit c)) . 
  foldr (\(c,_) g -> setCoord c ((findAt g c)+1) g) igrid 
  $ bs 
  where igrid = replicate my (replicate mx 0) 
        hx = mx `div` 2
        hy = my `div` 2

--- Parsing

parse :: String -> Bot
parse s = case matchRegex (mkRegex "p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)") s of
            Just strs -> let [x,y,vx,vy] = map (read::String->Int) strs in 
                            ((x,y),(vx,vy))
            _ -> ((0,0),(0,0)) -- shouldn't happen
