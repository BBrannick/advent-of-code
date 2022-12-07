import System.IO
import Data.List
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  run "input.txt" day7

demo = do
  run "input-demo.txt" day7

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents

data Node = Dir String [Node] | File String Int deriving (Show, Eq) 
data Cmd = CD String | LS [Node] deriving (Show, Eq)

day7 = pt2

pt1 = sum . filter (<= 100000) . map snd . dirSizes . parse
pt2 ls = minimum . filter (>= required) . map snd . dirSizes $ ns
  where required = tgt - (total - nSize ns)
        ns = parse ls

total = 70_000_000
tgt = 30_000_000


dirSizes :: Node -> [(String,Int)]
dirSizes (File _ _) = []
dirSizes d@(Dir s ns) = (s, nSize d):(concat $ map dirSizes ns)

nSize :: Node -> Int
nSize (File _ size) = size
nSize (Dir s ns) = sum (map nSize ns)

----------------------------
  --PARSING
-----------

parse :: [String] -> Node
parse ls = fst $ foldl
  (\(node,_) cmds -> parse' node cmds)
  ((Dir "/" []),[])
  (splitWhen (==(CD "/")) (parseInput ls))

parse' :: Node -> [Cmd] -> (Node, [Cmd])
parse' d [] = (d, [])
parse' (Dir s _) ((LS is):cmds) = parse' (Dir s is) cmds
parse' dir@(Dir s ns) ((CD d):cmds) 
  | d == ".." = (dir, cmds)
  | otherwise = parse' (Dir s (d':(filter (not.named d) ns))) cmds'
  where (d',cmds') = parse' (findSubdir d dir) cmds

findSubdir :: String -> Node -> Node
findSubdir s (File _ _) = error "nope"
findSubdir s (Dir n ns) = fromJust $ find (named s) ns

named :: String -> Node -> Bool
named a (Dir a' _) = a == a'
named _ (File _ _) = False

parseInput :: [String] -> [Cmd]
parseInput [] = []
parseInput (x:xs) = case parseCmd x of
                    CD n -> (CD n):(parseInput xs)
                    LS _ -> (LS (map parseItem items)):(parseInput rest)
                      where items = takeWhile ((/= '$').head) xs
                            rest = drop (length items) xs

parseItem :: String -> Node
parseItem s
  | take 3 s == "dir" = Dir (drop 4 s) []
  | otherwise = let (size:name:_) = words s in File name (read size::Int)

parseCmd :: String -> Cmd
parseCmd s = if (take 2 (drop 2 s)) == "cd"
                then CD (drop 5 s)
                else LS []

applyIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyIf f g [] = []
applyIf f g (x:xs) 
  | f x = (g x):(applyIf f g xs)
  | otherwise = x:(applyIf f g xs)

pretty :: Node -> IO ()
pretty = pretty' 0

pretty' :: Int -> Node -> IO ()
pretty' n (File x s) = putStrLn $ (replicate (n*2) ' ') ++ x ++ (' ':show s)
pretty' n (Dir x ns) = do
  putStrLn $ (replicate (n*2) ' ') ++ x
  mapM_ (pretty' (n+1)) ns

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f [] = []
splitWhen f xs = chunk:(splitWhen f rest)
  where chunk = takeWhile (not.f) xs
        rest = drop (1 + (length chunk)) xs

