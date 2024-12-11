module Day11 (pt1, pt2) where

pt1 :: String -> Int
pt1 = length . (!!25) . iterate next . parse

pt2 :: String -> Int
pt2 = length . (!!75) . iterate next . parse

parse :: String -> [Int]
parse = map (read::String->Int) . words

next :: [Int] -> [Int]
next xs = foldr (\x acc -> (stones x) ++ acc) [] xs

stones :: Int -> [Int]
stones 0 = [1]
stones x
  | even (length (show x)) = map (read::String->Int) . unTuple . bisect $ show x
  | otherwise = [2024*x]

unTuple :: (a,a) -> [a]
unTuple (x,y) = [x,y]

bisect :: [a] -> ([a],[a])
bisect xs = splitAt ((length xs) `div` 2) xs
