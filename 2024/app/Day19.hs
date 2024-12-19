module Day19 (pt1, pt2) where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M

pt1 :: String -> Int
pt1 s = length . filter ((>0) . possible ts) $ tgts
  where (ts,tgts) = parse s

pt2 :: String -> Int
pt2 s = sum . map (possible ts) $ tgts
  where (ts,tgts) = parse s

possible :: [String] -> String -> Int
possible ts = snd . possibleCached M.empty ts

possibleCached :: M.Map String Int -> [String] -> String -> (M.Map String Int, Int)
possibleCached cache ts t = case M.lookup t cache of 
                              Just n -> (cache,n)
                              _ -> (M.insert t totSum cache', totSum)
                                where (cache', totSum) = possibleCached' cache ts t

possibleCached' :: M.Map String Int -> [String] -> String -> (M.Map String Int, Int)
possibleCached' c _ "" = (c,1)
possibleCached' cache ts t = (cache',totSum) 
  where (cache',totSum) = foldr (\t' (aggCache,aggSum) -> fmap (+aggSum) (possibleCached aggCache ts t')) (cache,0) $ matches
        matches = catMaybes . map (`stripPrefix` t) $ ts

parse :: String -> ([String], [String])
parse s = (twls', rest')
  where (twls,rest) = fromMaybe ("", []) . uncons . lines $ s
        twls' = splitOn ", " twls
        rest' = snd . fromMaybe ("", []) . uncons $ rest
