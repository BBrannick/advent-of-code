module Day22 (pt1,pt2) where

import Data.Bits
import qualified Data.Map as M

type Incs = [Int]

pt1 :: String -> Int
pt1 = sum . map ((!!2000) . iterate nextSecret) . parse

pt2 :: String -> Int
pt2 = maximum . M.elems . M.unionsWith (+) . map (mapIncs . take 2001 . iterate nextSecret) . parse

nextSecret :: Int -> Int
nextSecret = prunemix (*2048) . prunemix (`div` 32) . prunemix (*64)

prunemix :: (Int -> Int) -> Int -> Int
prunemix f n = prune . xor n $ f n

prune :: Int -> Int
prune n = n `mod` 16777216

parse :: String -> [Int]
parse = map (read::String->Int).lines

mapIncs :: [Int] -> M.Map Incs Int
mapIncs secrets = mapIncs' pRaises
  where prices = map (`mod` 10) secrets
        pRaises = map (\(p,p') -> (p',p'-p)) $ zip prices (drop 1 prices)

mapIncs' :: [(Int,Int)] -> M.Map Incs Int
mapIncs' (a:b:c:d:rest) = M.insertWith const (map snd [a,b,c,d]) (fst d) $ mapIncs' (b:c:d:rest)
mapIncs' _ = M.empty
