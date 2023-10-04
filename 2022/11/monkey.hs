module Monkey where

data Op = Sqr | Mul | Add deriving (Eq,Show)

data Monkey = Monkey { mid :: Int
                     , items :: [Int]
                     , op :: Op
                     , arg :: Int
                     , test :: Int
                     , monkeyT :: Int
                     , monkeyF :: Int
                     , count :: Int
                     }

instance Show Monkey where
  show (Monkey { mid=id, items=i, op=_, arg=a, test=t, monkeyT=mt, monkeyF=mf , count=c}) = 
    concat ["Monkey "
           ,show id
           ," {items =\""
           ,show i
           ,"\",arg=\""
           ,show a
           ,"\",test=\""
           ,show t
           ,"\",monkeyT =\""
           ,show mt
           ,"\",monkeyF =\""
           ,show mf
           ,"\",count =\""
           ,show c
           ,"\"}"
           ]

