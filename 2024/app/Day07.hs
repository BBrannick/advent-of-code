module Day07 (pt1, pt2) where

pt1 :: String -> Int
pt1 = sum . map fst . filter (valid [(*),(+)]) . map parse .lines

pt2 :: String -> Int
pt2 = sum . map fst . filter (valid [(*),(+),cat]) . map parse .lines

---
---

parse :: String -> (Int,[Int])
parse str = (readInt (init tgt), map readInt ns)
  where (tgt:ns) = words str

readInt :: String -> Int
readInt = read::String->Int

---

valid :: [(Int -> Int -> Int)] -> (Int,[Int]) -> Bool
valid fs (s,ns) = s `elem` (results fs [] ns)

results :: [(Int -> Int -> Int)] -> [Int] -> [Int] -> [Int]
results _ ans [] = ans
results fs [] (x:xs) = results fs [x] xs
results fs ans (x:xs) = results fs ans' xs
  where ans' = concatMap (\f -> map ((flip f) x) ans) fs

cat :: Int -> Int -> Int
cat a b = readInt $ concat [show a, show b]

