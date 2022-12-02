import System.IO
import System.Environment
import Data.Char

-- Rock = 0; Paper = 1; Scissors = 2
type RPS = Int
-- Lose = 0; Draw = 1; Win = 2
type WLD = Int
type Round = (RPS, WLD)

main :: IO ()
main = do
	let file = "input.txt"
	run file

demo :: IO ()
demo = do
	let file = "input-demo.txt"
	run file

run filename = do
	ls <- getLines filename
	print $ rps . map parse $ ls

getLines :: String -> IO ([String])
getLines filename = do
	handle <- openFile filename ReadMode
	contents <- hGetContents handle
	return $ lines contents

rps :: [Round] -> Int
rps = sum . map rpsRound

rpsRound :: Round -> Int
rpsRound r = choice (move r) + win (snd r)

choice = (+1)

win = (*3)

move :: Round -> RPS
move (x, wld) = (x + wld + 2) `mod` 3

parse :: String -> Round
parse (x:_:x':_) = (parseRps x, parseWld x')

parseRps = subtract (ord 'A') . ord

parseWld = subtract (ord 'X') . ord


