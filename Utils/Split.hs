module Utils.Split where

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = x':(chunk n xs')
  where (x', xs') = splitAt n xs

