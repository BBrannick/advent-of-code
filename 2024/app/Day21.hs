module Day21(pt1, pt2, main) where

import Control.Monad

pt1 :: String -> Int
pt1 input = sum $ map (\l -> complexity l . dirpadString . dirpadString . numpadString $ l) ls
  where ls = lines input

pt2 :: String -> Int
pt2 input = length input

main :: IO ()
main = do 
  input <- readFile "data/21_demo.txt"
  _ <- forM (lines input) (\l -> do
    let str = dirpadString . dirpadString . numpadString $ l
        out = concat [l, ": ", str, "\nlength: ",show (length str)]
    putStrLn out
                          )
  return ()

complexity :: String -> String -> Int
complexity line presses = (length presses) * ((read::String->Int) (init line))

numpadString :: String -> String
numpadString s = foldr (\(a,b) acc -> (numSteps a b) ++ acc) "" $ zip ('A':s) s

numSteps :: Char -> Char -> String
numSteps a b = nsc (nCoords a) (nCoords b) True

dirpadString :: String -> String
dirpadString s = foldr (\(a,b) acc -> (dirSteps a b) ++ acc) "" $ zip ('A':s) s

dirSteps :: Char -> Char -> String
dirSteps a b = nsc (dCoords a) (dCoords b) False

--numpadStepsCoordinates
nsc :: (Int,Int) -> (Int,Int) -> Bool -> String
nsc (x,y) (x',y') flip = concat 
  [ ""
  , if y /= 0 && x' < x then replicate (x-x') '<' else [] 
  , if y' < y then replicate (y-y') (if flip then 'v' else '^') else []
  , if y' > y then replicate (y'-y) (if flip then '^' else 'v') else []
  , if x' > x then replicate (x'-x) '>' else []
  , if y == 0 && x' < x then replicate (x-x') '<' else [] 
  , "A"
  ]

fromNumString :: String -> String
fromNumString = fds (2,3) nCoords'

fromDirString :: String -> String
fromDirString = fds (2,0) dCoords'

fds :: (Int,Int) -> ((Int,Int) -> Char) -> String -> String
fds _ _ "" = ""
fds co f ('A':cs) = (f co):(fds co f cs)
fds (x,y) f (c:cs) = fds (case c of
                     '^' -> (x,y-1)
                     '>' -> (x+1,y)
                     'v' -> (x,y+1)
                     '<' -> (x-1,y)
                     _ -> error "AAA"
                       ) f cs


-- sometimes stupid is effective
-- reversed coords so we treat numpads the same
nCoords :: Char -> (Int,Int)
nCoords '7' = (0,3)
nCoords '8' = (1,3)
nCoords '9' = (2,3)
nCoords '4' = (0,2)
nCoords '5' = (1,2)
nCoords '6' = (2,2)
nCoords '1' = (0,1)
nCoords '2' = (1,1)
nCoords '3' = (2,1)
nCoords '0' = (1,0)
nCoords 'A' = (2,0)
nCoords _ = error "invalid numpad character"

-- sometimes stupid is effective
nCoords' :: (Int,Int) -> Char
nCoords' (0,0) = '7'
nCoords' (1,0) = '8'
nCoords' (2,0) = '9'
nCoords' (0,1) = '4'
nCoords' (1,1) = '5'
nCoords' (2,1) = '6'
nCoords' (0,2) = '1'
nCoords' (1,2) = '2'
nCoords' (2,2) = '3'
nCoords' (1,3) = '0'
nCoords' (2,3) = 'A'
nCoords' _ = error "invalid numpad character"

dCoords :: Char -> (Int,Int)
dCoords '^' = (1,0)
dCoords 'A' = (2,0)
dCoords '<' = (0,1)
dCoords 'v' = (1,1)
dCoords '>' = (2,1)
dCoords _ = error "invalid dirpad character"

dCoords' :: (Int,Int) -> Char
dCoords' (1,0) = '^'
dCoords' (2,0) = 'A' 
dCoords' (0,1) = '<'
dCoords' (1,1) = 'v'
dCoords' (2,1) = '>'
dCoords' _ = error "invalid dirpad character"
