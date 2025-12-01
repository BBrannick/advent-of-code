module Day01 (pt1, pt2) where

pt1 :: String -> Int
pt1 = length . filter ((==0) . (`mod` 100)) . positions 50 . map parseTurn . lines 

pt2 :: String -> Int
pt2 = snd . foldr makeTurn (50,0) . reverse . map parseTurn . lines 

positions :: Int -> [Int] -> [Int]
positions n xs = reverse . foldr (\t ps@(p:_) -> ((p+t):ps)) [n] . reverse $ xs

makeTurn :: Int -> (Int,Int) -> (Int,Int)
makeTurn t (p,c) =  (m, c + (abs d) + negativeModOffset + fromZeroOffset)
  where (d,m) = (p+t) `divMod` 100
        negativeModOffset = (if d <= 0 && m == 0 then 1 else 0)
        fromZeroOffset = (if t < 0 && p == 0 then -1 else 0)

--

parseTurn :: String -> Int
parseTurn ('L':xs) = (-1) * ((read::String->Int) xs)
parseTurn (_:xs) = (read::String->Int) xs
