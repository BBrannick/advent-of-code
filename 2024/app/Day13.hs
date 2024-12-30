module Day13 (pt1, pt2) where

import Data.List.Split (splitOn)
import Text.Regex
import Data.Maybe

-- X, Y
type Coord = (Int,Int)
-- A press, B press, Target
type Mach = (Coord, Coord, Coord)

pt1 :: String -> Int
pt1 = sum . catMaybes . map cost . parse 0

-- Solved with help from https://advent-of-code.xavd.id/writeups/2024/day/13/
pt2 :: String -> Int
pt2 = sum . catMaybes . map cost . parse 10000000000000

cost :: Mach -> Maybe Int
cost ((ax,ay),(bx,by),(tx,ty)) = if a_rem == 0 && b_rem == 0 
                                    then Just $ (3*a_presses) + b_presses 
                                    else Nothing
  where (a_presses,a_rem) = divMod ((tx*by)-(ty*bx)) ((ax*by)-(ay*bx))
        (b_presses,b_rem) = divMod (tx - (a_presses * ax)) bx

  {- Initial idea was here
      Idea was to find the intersection of B intersecting with (0,0), and A 
      intersecting with the target, then check that B could reach that point in 
      an integer number of presses, and A could reach the target in an integer
      number of presses.
      That answer was too low, I'm still not certain why, but the above
      solution is cleaner anyway. I've left this here for posterity
      -}
_fullCost :: Mach -> Maybe Int
_fullCost (a,b,t) = case (costIA,costBI) of
                     (Just ca, Just cb) -> Just (ca+cb)
                     _ -> Nothing
  where is = intersection a b t
        costBI = cost' 1 (0,0) b is
        costIA = cost' 3 is a t

-- Find the cost of pressing button to move between two coords, or nothing if impossible
cost' :: Int -- cost of one button press
      -> Coord -- starting coordinate
      -> Coord -- vector
      -> Coord -- target coordinate
      -> Maybe Int -- cost if possible, nothing otherwise
cost' c (x,y) (vx,vy) (tx,ty)
  | rx == 0 && ry == 0 && dx == dy = Just (c*dx)
  | otherwise = Nothing
  where tgtX = tx-x
        tgtY = ty-y 
        (dx,rx) = divMod tgtX vx 
        (dy,ry) = divMod tgtY vy

intersection :: Coord -> Coord -> Coord -> Coord
intersection (ax,ay) (bx,by) (tx,ty) = (ix,iy)
  where denom = (fromIntegral::Int->Double) $ (by*ax)-(ay*bx)
        c = (fromIntegral ty) - ((fromIntegral tx) * (((fromIntegral::Int->Double) ay)/(fromIntegral ax)))
        ix = floor $ ((fromIntegral bx)*(fromIntegral ax)*c) / denom
        iy = floor $ ((fromIntegral by)*(fromIntegral ax)*c) / denom

--- PARSING LOGIC

parse :: Int -> String -> [Mach]
parse offset = map (parseMach offset) . init . splitOn "\n\n"

parseMach :: Int -> String -> Mach
parseMach offset s = case zipWith ($) [parseBtn, parseBtn, parseTgt offset] (lines s) of
                       [a,b,t] -> (a,b,t)
                       _ -> error "failed to parse"

parseBtn :: String -> Coord
parseBtn = parseLn (mkRegex "X\\+([0-9]+), Y\\+([0-9]+)")

parseTgt :: Int -> String -> Coord
parseTgt offset s = (x+offset,y+offset)
  where (x,y) = parseLn (mkRegex "X=([0-9]+), Y=([0-9]+)") s

parseLn :: Regex -> String -> Coord
parseLn r s = case matchRegex r s of
               Just [x,y] -> ((read::String->Int) x, (read::String->Int) y)
               _ -> error "failed to parse"


