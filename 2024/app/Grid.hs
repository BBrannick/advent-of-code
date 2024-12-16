module Grid ( 
  Grid,
  Coord,
  fromString,
  intsFromString,
  bounds,
  inBounds,
  findAt,
  findAll, 
  setCoord,
  draw
            ) where

import Data.Char (digitToInt)

type Grid a = [[a]]
type Coord = (Int,Int)

fromString :: String -> Grid Char
fromString = lines

intsFromString :: String -> Grid Int
intsFromString = map (map digitToInt) . lines

bounds :: Grid a -> Coord
bounds g = (length (head g) - 1, length g - 1)

inBounds :: Grid a -> Coord -> Bool
inBounds g (x,y) = 0 <= y && y <= my && 0 <= x && x <= mx
  where (mx,my) = bounds g

findAt :: Grid a -> Coord -> a
findAt g (x,y) = g!!y!!x

findAll :: Eq a => Grid a -> a -> [Coord]
findAll g c = [(x,y) | x <- [0..mx], y <- [0..my], g!!y!!x == c]
  where (mx,my) = bounds g

setCoord :: Coord -> a -> Grid a -> Grid a
setCoord (x,y) c m = rsPre ++ ((rPre ++ (c:rPost)):(rsPost))
  where (rsPre, (row:rsPost)) = splitAt y m
        (rPre, (_:rPost)) = splitAt x row

draw :: Grid Char -> IO ()
draw = mapM_ putStrLn 
