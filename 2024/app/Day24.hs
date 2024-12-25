module Day24 (pt1,pt2) where

import qualified Data.Map as M
import Data.List
import Data.Maybe

data Gate = AND | OR | XOR deriving (Eq,Show,Read)
data Tree a b = Leaf a b | Node a Gate (Tree a b) (Tree a b) deriving (Eq,Show)
type Deps = Either Bool (Gate,Id,Id)
type Id = String


pt1 :: String -> Int
pt1 = boolsToInt . map evalTree . parse

  {-
     For part 2 I built the rules map and checked in a console for zXX pins 
     where one input wasn't xXX XOR yXX.
     I also checked the dependencies of each zXX pin, and found z10 only 
     depended on x10 and y10, and none of the previous x or y pins
     After that I fixed the input according to those swaps, then added all
     two all-1 bit numbers to find the incorrect register.
     I don't remember the exact bit that was incorrect, but there was an OR 
     where an AND should have been, or vice versa
     -}
pt2 :: String -> Int
pt2 _ = 0

parse :: String -> [Tree Id Bool]
parse str = toTrees fullRules
  where (is,rest) = span (not.null) $ lines str
        initials = M.map (Left) $ M.fromList (map parseInit is)
        rules = M.map (Right) $ M.fromList (map parseRule (drop 1 rest))
        fullRules = M.union initials rules

parseInit :: String -> (Id,Bool)
parseInit [a,b,c,':',' ',bit] = ([a,b,c],bit == '1')
parseInit _ = error "cannot parse"

parseRule :: String -> (Id,(Gate,Id,Id))
parseRule s = case words s of
                [id1,gate,id2,_,i] -> (i, ((read::String->Gate) gate, id1, id2))
                _ -> error "cannot parse"

toTrees :: M.Map Id Deps -> [Tree Id Bool]
toTrees rules = map (toTree rules) zs
  where
    zs = filter ((=="z") . take 1) . sort $ M.keys rules

toTree :: M.Map Id Deps -> Id -> Tree Id Bool
toTree rules i = case fromJust (M.lookup i rules) of
                    Left bool -> Leaf i bool
                    Right (gate,i1,i2) -> Node i gate treeL treeR
                      where treeL = toTree rules i1
                            treeR = toTree rules i2
                    
evalTree :: Tree Id Bool -> Bool
evalTree (Leaf _ b) = b
evalTree (Node _ g l r) = eval g (evalTree l) (evalTree r)

eval :: Gate -> Bool -> Bool -> Bool
eval AND = (&&)
eval OR = (||)
eval XOR = (/=)

boolsToInt :: [Bool] -> Int
boolsToInt [] = 0
boolsToInt (b:bs) = (if b then 1 else 0) + (2 * boolsToInt bs)

left :: Tree a b -> Tree a b
left (Leaf _ _) = error "left on empty tree"
left (Node _ _ t _) = t

right :: Tree a b -> Tree a b
right (Leaf _ _) = error "right on empty tree"
right (Node _ _ _ t) = t

deps :: Eq a => Tree a b -> [a]
deps (Leaf i _ ) = [i]
deps (Node _ _ l r) = nub $ deps l ++ deps r

name :: Tree a b -> a
name (Leaf i _) = i
name (Node i _ _ _) = i

