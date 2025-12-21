module Day11 (pt1, pt2) where

import Data.Maybe
import qualified Data.HashMap as M
import Debug.Trace

type Dev = String
type DevMap = M.Map Dev [Dev]

pt1 :: String -> Int
pt1 = fromJust . countPaths "you" "out" . parse

pt2 :: String -> Int
pt2 str = (svrToFft * fftToDac * dacToOut) + (svrToDac * dacToFft * fftToOut)
  where m = parse str
        svrToFft = fromMaybe 0 $ countPaths "svr" "fft" (M.delete "dac" m)
        svrToDac = fromMaybe 0 $ countPaths "svr" "dac" (M.delete "fft" m)
        fftToDac = fromMaybe 0 $ countPaths "fft" "dac" m
        dacToFft = fromMaybe 0 $ countPaths "dac" "fft" m
        fftToOut = fromMaybe 0 $ countPaths "fft" "out" (M.delete "dac" m)
        dacToOut = fromMaybe 0 $ countPaths "dac" "out" (M.delete "fft" m)

countPaths :: Dev -> Dev -> DevMap -> Maybe Int
countPaths d tgt m = fst (countPaths' d m (M.singleton tgt (Just 1)))

countPaths' :: Dev -> DevMap -> M.Map Dev (Maybe Int) -> (Maybe Int, M.Map Dev (Maybe Int))
countPaths' d devMap paths
  | M.member d paths = (fromJust (M.lookup d paths), paths)
  | not (M.member d devMap) = (Nothing, M.insert d Nothing paths)
  | otherwise = (mNumPaths, M.insert d mNumPaths newPaths)
    where ds = fromJust (M.lookup d devMap)
          devMap' = M.delete d devMap
          (mNumPaths, newPaths) = foldr (\d' (mi,pathAcc) -> 
            let (mi',paths') = countPaths' d' devMap' pathAcc 
            in (addMaybe mi mi', paths')
            ) (Nothing, paths) ds


addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe Nothing Nothing = Nothing
addMaybe Nothing x = x
addMaybe x Nothing = x
addMaybe (Just x) (Just y) = Just (x+y)

parse :: String -> DevMap
parse = M.fromList . map parse' . lines

-- Parse a line. Relies on devices being exactly 3 characters
parse' :: String -> (Dev, [Dev])
parse' (a:b:c:':':rest) = (a:b:[c], words rest)
parse' _ = error "bad parse"
