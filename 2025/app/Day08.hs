module Day08 (pt1, pt2) where

import Data.Maybe
import Data.List
import Data.Ord
import Data.List.Split
import qualified Data.HashMap as M
import Data.Hashable

type Vec = (Int,Int,Int)

pt1 :: String -> Int
pt1 str = product . take 3 . countCircuits . buildNet numConn $ str
  where numConn = if length str > 1000 then 1000 else 10

pt2 :: String -> Int
pt2 = length

countCircuits :: M.Map Vec Vec -> [Int]
countCircuits m = reverse . sort . M.elems . tally . M.elems $ m

tally :: (Ord a, Hashable a) => [a] -> M.Map a Int
tally = M.fromListWith (+) . flip zip (repeat 1)

buildNet :: Int -> String -> M.Map Vec Vec
buildNet n ls = foldl' addConn M.empty . map snd . take n . sortBy (comparing fst) $ distances
  where pairs = allPairs . map parse . lines $ ls
        distances = zip (map (uncurry dist) pairs) pairs

{-
countNets :: M.Map Vec [Vec] -> Int
countNets m = cn m []

cn map seen

addConn :: M.Map Vec [Vec] -> (Vec,Vec) -> M.Map Vec [Vec]
addConn m (v1,v2) = M.alter (doAdd v1) v2 (M.alter (doAdd v2) v1 m)
  where doAdd v = (\x -> case x of 
                           Just vs -> Just (v:vs)
                           Nothing -> Just [v])
 -}

addConn :: M.Map Vec Vec -> (Vec,Vec) -> M.Map Vec Vec
addConn m (v1,v2) = if (isJust v1Net && v1Net == v2Net) then m else M.insert v1 newNet updated
  where v1Net = (M.lookup v1 m)
        v2Net = (M.lookup v2 m)
        newNet = fromMaybe v1 . listToMaybe . catMaybes $ [v1Net, v2Net]
        toUpdate = v2:(M.elems . M.filter ((==v2Net) . Just) $ m)
        updated = foldr (flip M.insert newNet) m toUpdate
        


-- Not technically the correct distance, but since we're only comparing distances we don't need the sqrt
dist :: Vec -> Vec -> Int
dist (x1,y1,z1) (x2,y2,z2) = sum . map (\(p,q) -> (p-q)^2) $ [(x1,x2),(y1,y2),(z1,z2)]

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = ((repeat x) `zip` xs) ++ allPairs xs

parse :: String -> Vec
parse str = case splitOn "," str of
              (x:y:z:_) -> (readI x, readI y, readI z)
              _ -> error "Bad string"
  where readI = read::String->Int
