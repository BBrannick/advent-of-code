module Day17 (pt1, pt2) where

import Data.Bits
import Data.Char
import Data.List
import Data.Maybe

data Ins = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Eq,Show,Enum)
type State = (Int,(Int,Int,Int),[Int])

pt1 :: String -> String
pt1 input = intercalate "," . reverse . map show $ outs
  where (ops,state) = parse input
        (_,_,outs) = step ops state

pt2 :: String -> Int
pt2 input = findA (reverse ops)
  where (ops,_) = parse input


findA :: [Int] -> Int
findA = fromJust . findA' 0

findA' :: Int -> [Int] -> Maybe Int
findA' prefix [] = Just prefix
findA' prefix (op:ops) = listToMaybe . catMaybes . map (\c -> findA' c ops) . filter ((==op) . calcA) . map (+(prefix*8)) $ [0..7]

-- Given an input in register A, work out the number that would be output by an OUT op
-- hard-coded to my input
calcA :: Int -> Int
calcA a = (((a `mod` 8) `xor` 3) `xor` 5) `xor` (a `shiftR` ((a `mod` 8) `xor` 3)) `mod` 8

step :: [Int] -> State -> State
step ops s@(i,_,_) 
  | i >= length ops = s
  | otherwise = step ops s'
    where s' = doIns ((toEnum::Int->Ins) (ops!!i)) (ops!!(i+1)) s

doIns :: Ins -> Int -> State -> State
doIns ADV o s@(_,(a,_,_),_) = incI . setA (a `shiftR` (combo s o)) $ s
doIns BXL o s@(_,(_,b,_),_) = incI . setB (b `xor` o) $ s
doIns BST o s = incI . setB ((combo s o) `mod` 8) $ s
doIns JNZ o s@(_,(a,_,_),_)
  | a == 0 = incI s 
  | otherwise = setI o s
doIns BXC _ s@(_,(_,b,c),_) = incI . setB (b `xor` c) $ s
doIns OUT o s@(i,regs,outs) = incI (i,regs,out:outs)
  where out = (combo s o) `mod` 8
doIns BDV o s@(_,(a,_,_),_) = incI . setB (a `shiftR` (combo s o)) $ s
doIns CDV o s@(_,(a,_,_),_) = incI . setC (a `shiftR` (combo s o)) $ s

setA :: Int -> State -> State
setA n (i,(_,b,c),outs) = (i,(n,b,c),outs)

setB :: Int -> State -> State
setB n (i,(a,_,c),outs) = (i,(a,n,c),outs)

setC :: Int -> State -> State
setC n (i,(a,b,_),outs) = (i,(a,b,n),outs)

setI :: Int -> State -> State
setI n (_,regs,outs) = (n,regs,outs)

incI :: State -> State
incI s@(i,_,_) = setI (i+2) s

combo :: State -> Int -> Int
combo (_,(a,b,c),_) i = case i of
                        4 -> a
                        5 -> b
                        6 -> c
                        _ -> i

parse :: String -> ([Int],State)
parse str = (ops,(0,regs,[]))
  where ls = lines str
        regs = case map ((read::String->Int) . dropWhile (not.isDigit)) $ take 3 ls of
                 [a,b,c] -> (a,b,c)
                 _ -> error "cannot parse registers"
        ops = maybe (error "failure to parse") (map digitToInt . filter isDigit . fst) . uncons . drop 1 . words $ ls!!4
