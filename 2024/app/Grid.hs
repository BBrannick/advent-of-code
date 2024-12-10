module Grid (fromString, intsFromString, inBounds, findAll) where

import Data.Char (digitToInt)

type Grid a = [[a]]
type Coord = (Int,Int)

fromString :: String -> Grid Char
fromString = lines

intsFromString :: String -> Grid Int
intsFromString = map (map digitToInt) . lines

inBounds :: Grid a -> Coord -> Bool
inBounds m (x,y) = 0 <= y && y < (length m) && 0 <= x && x < (length (head m))

findAll :: Eq a => Grid a -> a -> [Coord]
findAll g c = [(x,y) | x <- [0..(length (head g) - 1)], y <- [0..length g - 1], g!!y!!x == c]

