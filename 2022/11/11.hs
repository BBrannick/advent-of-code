import Monkey
import ParseMonkey
import System.IO
import qualified Data.Map as M
import Data.List

main :: IO ()
main = do
  run "input.txt" day11

demo = do
  run "input-demo.txt" day11

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

day11 = pt1

pt1 = product . take 2 . reverse . sort . map count . M.elems . doRounds 10000 . parse

doRounds :: Int -> M.Map Int Monkey -> M.Map Int Monkey
doRounds i m = iterate doRound m !! i

doRound :: M.Map Int Monkey -> M.Map Int Monkey
doRound ms = foldl (flip takeTurnAndThrow) ms [0..(length ms - 1)]

takeTurnAndThrow :: Int -> M.Map Int Monkey -> M.Map Int Monkey
takeTurnAndThrow i map = M.adjust dropItems i ( foldr throw map throws )
  where throws = takeTurn (map M.! i)

throw :: (Int,[Int]) -> M.Map Int Monkey -> M.Map Int Monkey
throw (m,items) map = M.adjust (receive items) m map

receive :: [Int] -> Monkey -> Monkey
receive is' m@(Monkey {items = is}) = m { items = (is ++ is') }

dropItems :: Monkey -> Monkey
dropItems m@(Monkey {items = is, count = c}) = m {items = [], count = (c + length is)}

takeTurn :: Monkey -> [(Int,[Int])]
takeTurn m = throwAll (monkeyT m) (monkeyF m) $ map (worryM m) (items m)  

worry :: (Int -> Int) -> Int -> Int
--worry f x = (f x) `div` 3
worry f x = (f x) 

worryM :: Monkey -> Int -> Int
worryM (Monkey {op=o, arg=a, test=t}) x
  | o == Sqr = ((x `mod` t) ^ 2) `mod` t
  | o == Mul = ((x `mod` t) * (a `mod` t)) `mod` t
  | otherwise =  ((x `mod` t) + (a `mod` t)) `mod` t

throwAll :: Int -> Int -> [Int] -> [(Int,[Int])]
throwAll mt mf xs = [(mt,ts),(mf,fs)]
  where (ts,fs) = place (== 0) xs

place :: (a -> Bool) -> [a] -> ([a],[a])
place f xs = foldr (\x (ts,fs) -> if (f x) then (x:ts,fs) else (ts,x:fs)) ([],[]) xs

inspect :: (Int -> Int) -> (Int -> Bool) -> Int -> Int -> Int -> (Int,Int)
inspect op test mt mf item = (m,item')
  where m = if test item' then mt else mf
        item' = (op item) `div` 3

parse :: [String] -> M.Map Int Monkey
parse = M.fromList . zip [0..] . map parseM . chunk 7 

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = x':(chunk n xs')
  where (x', xs') = splitAt n xs
