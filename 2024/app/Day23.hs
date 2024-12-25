module Day23 (pt1, pt2, main) where

import qualified Data.Map as M
import Data.List
import qualified Data.Set as S
import Debug.Trace

type Id = String

pt1 :: String -> Int
pt1 input = length . nub . concatMap (links3 cns) . filter ("t" `isPrefixOf`) $ M.keys cns
  where cns = connections input

pt2 :: String -> Int
pt2 = length

main :: IO ()
main = do
  putStrLn "hello"

links3 :: M.Map Id [Id] -> Id -> [[Id]]
links3 cns id = nub . map sort . concatMap (\id' -> map (\id'' -> [id,id',id'']) . filter (elem id . links) . links $ id') $ links id
  where links = (\l -> M.findWithDefault [] l cns)

connections :: String -> M.Map Id [Id]
connections = foldr (\(a,b) -> M.insertWith (++) a [b] . M.insertWith (++) b [a]) M.empty . map parse . lines

parse :: String -> (Id,Id)
parse [a,b,'-',c,d] = ([a,b],[c,d])
parse _ = error "cannot parse"

buildSets :: M.Map Id [Id] -> [Id] -> [S.Set Id]
buildSets cns ids = foldr (\i sets -> updateSets cns sets i) [] ids

updateSets :: M.Map Id [Id] -> [S.Set Id] -> Id -> [S.Set Id]
updateSets cns sets i = traceShowId$ conn' ++ other ++ newSet
  where (conn, other) = partition (\s -> connected cns s i) sets
        conn' = map (S.insert i) conn
        newSet = if null conn then [S.singleton i] else []

connected :: M.Map Id [Id] -> S.Set Id -> Id -> Bool
connected cns set i = set `S.isSubsetOf` (S.fromList ids)
  where ids = M.findWithDefault [] i cns

