import System.IO
import Data.Char

data Inst = Forward Int | Down Int | Up Int deriving (Show,Read)

main = do
	ls <- getInput
	let (x,y) = navigate . map parse $ ls
	    mul = x * y
	print (x,y) 
	print mul

navigate :: [Inst] -> (Int,Int)
navigate = nav' (0,0,0)

nav' :: (Int,Int,Int) -> [Inst] -> (Int,Int)
nav' (x,y,_) [] = (x,y)
nav' (x,y,a) (Forward x':is) = nav' (x + x',y + (x' * a),a) is
nav' (x,y,a) (Down y':is) = nav' (x,y,a + y') is
nav' (x,y,a) (Up  y':is) = nav' (x,y,a - y') is

parse :: String -> Inst
parse (x:xs) = read ((toUpper x):xs)::Inst

getInput :: IO ([String])
getInput = do
	handle <- openFile "input.txt" ReadMode
	contents <- hGetContents handle
	return $ lines contents


