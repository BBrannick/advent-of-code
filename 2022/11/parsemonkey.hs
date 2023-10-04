module ParseMonkey where

import Monkey

parseM :: [String] -> Monkey
parseM ss = Monkey {mid=m, items=is, op=o, arg=a, test=t, monkeyT=mt, monkeyF=mf, count=0}
  where (m,ss') = pipe parseMId ss
        (is,ss'') = pipe parseMItems ss'
        ((o,a),ss''') = pipe parseMOp ss''
        (t,ss'''') = pipe parseMTest ss'''
        (mt,ssV) = pipe parseMMonkey ss''''
        (mf,ssVI) = pipe parseMMonkey ssV

parseMId :: String -> Int
parseMId s = read (init ((words s)!!1))::Int

parseMItems :: String -> [Int]
parseMItems = map (read::String->Int) . reverse . sanscomma. reverse . drop 2 . words
  where sanscomma = (\(x:xs) -> x:(map init xs))

parseMOp :: String -> (Op, Int)
parseMOp s
  | op == "*" && arg == "old" = (Sqr, 2)
  | op == "*" = (Mul, nArg)
  | op == "+" = (Add, nArg)
  where ws = words s
        op = ws!!4
        arg = ws!!5
        nArg = read arg::Int

parseMMonkey :: String -> Int
parseMMonkey s = read ((words s)!!5)::Int

pipe :: (String -> a) -> [String] -> (a,[String])
pipe f (x:xs) = (f x, xs)

parseMTest :: String -> Int
parseMTest s = read ((words s)!!3)::Int
