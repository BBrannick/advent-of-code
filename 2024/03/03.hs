import System.IO
import Data.List
import Data.Maybe
import qualified Data.Map as Map


-- Boilerplate
main :: IO ()
main = do
  run "full.txt" main'

demo :: IO ()
demo = do
  run "demo.txt" main'

run filename fn = do
  ls <- getLines filename
  print $ fn ls

getLines :: String -> IO ([String])
getLines filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  return $ lines contents


-- Main
main' = pt1

pt1 :: [String] -> Int
pt1 = length
