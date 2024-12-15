module Day13 (pt1, pt2) where

import Data.List.Split (splitOn)
import Text.Regex
import Data.Maybe

-- X, Y
type Coord = (Int,Int)
-- A press, B press, Target
type Mach = (Coord, Coord, Coord)

pt1 :: String -> Int
pt1 s = sum . catMaybes . map cost . parse $ s

pt2 :: String -> Int
pt2 = length

cost :: Mach -> Maybe Int
cost (a,b@(x,y),tgt@(x',y')) = maybeMin .
  catMaybes . 
    map (\(b,ma) -> fmap (+b) ma) .
    zip [0..] .
      map (costA a) $
        (zip [x',(x'-x)..0] [y',(y'-y)..0])

maybeMin :: Ord a => [a] -> Maybe a
maybeMin [] = Nothing
maybeMin xs = Just (minimum xs)

costA :: Coord -> Coord -> Maybe Int
costA (x,y) (tgtX,tgtY)
  | rx == 0 && ry == 0 && dx == dy = Just (3*dx)
  | otherwise = Nothing
  where (dx,rx) = divMod tgtX x 
        (dy,ry) = divMod tgtY y


tu :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
tu f (x,y) (x',y') = (f x x', f y y')

--- PARSING LOGIC

parse :: String -> [Mach]
parse = map parseMach . init . splitOn "\n\n"

parseMach :: String -> Mach
parseMach s = (a,b,tgt)
  where [a,b,tgt] = zipWith ($) [parseBtn, parseBtn, parseTgt] (lines s)

parseBtn :: String -> Coord
parseBtn = parseLn (mkRegex "X\\+([0-9]+), Y\\+([0-9]+)")

parseTgt :: String -> Coord
parseTgt = parseLn (mkRegex "X=([0-9]+), Y=([0-9]+)")

parseLn :: Regex -> String -> Coord
parseLn r s = case matchRegex r s of
               Just [x,y] -> ((read::String->Int) x, (read::String->Int) y)
               _ -> (0,0) -- shouldn't happen


