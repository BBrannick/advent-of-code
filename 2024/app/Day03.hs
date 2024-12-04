module Day03 (pt1, pt2) where

import Text.Regex

pt1 :: String -> Int
pt1 = doMults . parseMults


pt2 :: String -> Int
pt2 = sum . map (doMults.parseMults) . parseDos 

doMults :: [(Int,Int)] -> Int
doMults = sum . map (uncurry (*))

parseDos :: String -> [String]
parseDos str = case matchRegexAll dontRegex str of
                 Nothing -> [str]
                 Just (str',_,rest,_) -> str':(parseDonts rest)

parseDonts :: String -> [String]
parseDonts str = case matchRegexAll doRegex str of
                 Nothing -> []
                 Just (_,_,rest,_) -> parseDos rest


parseMults :: String -> [(Int,Int)]
parseMults = parse' . matchRegexAll mulRegex 

parse' :: Maybe (String,String,String,[String]) -> [(Int,Int)]
parse' Nothing = []
parse' (Just (_,_,rest,(x:y:[]))) = (xInt,yInt):(parseMults rest)
  where xInt = (read::String->Int) x
        yInt = (read::String->Int) y
parse' _ = []

doRegex :: Regex
doRegex = mkRegex "do\\(\\)"

dontRegex :: Regex
dontRegex = mkRegex "don't\\(\\)"

mulRegex :: Regex
mulRegex = mkRegex "mul\\(([0-9]+),([0-9]+)\\)"

