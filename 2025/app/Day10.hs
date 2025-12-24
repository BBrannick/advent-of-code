module Day10 (pt1, pt2) where

import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import qualified Data.HashMap as M

type Mach = (Target,[Button],Jolts)
type Target = Int
type Button = [Int]
type Jolts = M.Map Int Int

pt1 :: String -> Int
pt1 = sum . map (findMin . parse ) . lines

findMin :: Mach -> Int
findMin (tgt, btns, _) = minimum . map length . filter ((==tgt) . press) . subsequences $ btns

press :: [Button] -> Target
press = foldr xor zeroBits . concatMap (map bit)

-----

pt2 :: String -> Int
pt2 ls = sum . map (\(_,bs,js) -> fromJust (foo bs js)) $ machs
  where machs = map parse . lines $ ls

foo :: [Button] -> Jolts -> Maybe Int
foo bs js
  | all (==0) js = Just 0
  | otherwise = case results of 
                  [] -> Nothing
                  _ -> Just (1 + minimum (results))
  where maxJ = keyOfMax js
        candidates = filter (elem maxJ) $ bs
        candidateJs = mapMaybe (flip pressJ js) candidates
        results = mapMaybe (foo bs) candidateJs

  {-
foo' :: [Button] -> [Int] -> Maybe Int
foo' bs js 
  | all (==0) js = Just 0
  | otherwise = case mapMaybe (foo' bs) (mapMaybe (flip pressJ js) bs) of
                  [] -> Nothing 
                  opts -> Just (minimum opts)
                  -}

pressJ :: Button -> Jolts -> Maybe Jolts
pressJ btns js = if any (<0) js' then Nothing else Just js'
  where js' = foldr (M.adjust (flip (-) 1)) js btns

keyOfMax :: Ord a => M.Map k a -> k
keyOfMax = fst . maximumBy (comparing snd) . M.toList

replace :: Int -> a -> [a] -> [a]
replace i x xs = pre ++ (x:post)
      where (pre, (_:post)) = splitAt i xs

-----

parse :: String -> Mach
parse s = (lights,buttons,jolts)
  where (ls, s') = fromJust (uncons (words s))
        (js, bs) = fromJust (uncons (reverse s'))
        lights = parse' ls
        buttons = map readInts bs
        jolts = M.fromList (zip [0..] (readInts js))

parse' :: String -> Target
parse' str = foldr xor zeroBits . map (bit . fst) . filter ((=='#') . snd) . zip [0..] $ (strip str)

readInts :: String -> [Int]
readInts = map (read::String->Int) . splitOn "," . strip

strip :: [a] -> [a]
strip = init . tail
