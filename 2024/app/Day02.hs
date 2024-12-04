module Day02 (pt1, pt2) where

pt1 :: [String] -> Int
pt1 = length . filter isSafe . map parse 

parse :: String -> [Int]
parse = map (read::String->Int) . words 

isSafe :: [Int] -> Bool
isSafe report@(x:y:_)
  | x < y = isSafe' safeInc report
  | x > y = isSafe' safeDec report
  | otherwise = False
isSafe _ = True

isSafe' :: (a -> a -> Bool) -> [a] -> Bool
isSafe' cond (x:y:rest) = (cond x y) && isSafe' cond (y:rest)
isSafe' _ _ = True

safeInc :: Int -> Int -> Bool
safeInc x y = x < y && y <= (x+3)

safeDec :: Int -> Int -> Bool
safeDec x y = x > y && y >= (x-3)

-- Pt 2
pt2 :: [String] -> Int
pt2 = length . filter isSafeish . map parse


isSafeish :: [Int] -> Bool
isSafeish reports = isSafe reports || any isSafe (sublists reports)

sublists :: [Int] -> [[Int]]
sublists xs = sublists' (length xs) xs

sublists' :: Int -> [Int] -> [[Int]]
sublists' 0 _ = []
sublists' n xs = ((take (n-1) xs) ++ (drop n xs)):(sublists' (n-1) xs)
