module Day15 (pt1,pt2) where

import Grid

type State = (Grid Char, Coord)

pt1 :: String -> Int
pt1 input = sum . map val . findAll grid $ 'O'
  where (s,ins) = parse input
        (grid,_) = foldl' step s ins

pt2 :: String -> Int
pt2 input = sum . map val . findAll grid $ '['
  where (s,ins) = parse' True input
        (grid,_) = foldl' step s ins

val :: Coord -> Int
val (x,y) = (100*y) + x

step :: State -> Char -> State
step (g,coord) d
  | dst == '#' = (g,coord)
  | dst == '.' = (g,coord')
  | dst == 'O' = case findSpace g coord' d of
                  Just cSpace -> (setCoord cSpace 'O' . setCoord coord' '.' $ g, coord')
                  Nothing -> (g,coord)
  | d == '>' = case moveBoxR g coord' of
                 Just g' -> (g',coord')
                 Nothing -> (g,coord)
  | d == '<' = case moveBoxL g (x',y') of
                 Just g' -> (g',coord')
                 Nothing -> (g,coord)
  | otherwise = let bc@(bx,by) = if dst == ']' then (x'-1,y') else coord' in 
                    case placeBox g (next bc d) d of 
                      Just g' -> (g'',coord') 
                        where g'' = setCoords g' [(bc,'.'),((bx+1,by),'.')]
                      Nothing -> (g,coord)
    where coord'@(x',y') = next coord d
          dst = findAt g coord'

findSpace :: Grid Char -> Coord -> Char -> Maybe Coord
findSpace g coord d
  | cand == '.' = Just coord
  | cand == '#' = Nothing
  | otherwise = findSpace g (next coord d) d
  where cand = findAt g coord

placeBox :: Grid Char -> Coord -> Char -> Maybe (Grid Char)
placeBox g c@(x,y) d
  | l == '#' || r == '#' = Nothing
  | l == '.' && r == '.' = Just (placeBox' g c)
  | l == '[' = placeBox g (next c d) d
  | l == ']' && r == '.' = 
    fmap (\g' -> placeBox' (setCoord (x-1,y) '.' g') c) (placeBox g (next (x-1,y) d) d)
  | l == '.' && r == '[' = 
    fmap (\g' -> placeBox' (setCoord (x+2,y) '.' g') c) (placeBox g (next cr d) d)
  | l == ']' && r == '[' = fmap (\g' -> placeBox' (setCoords g' [((x-1,y),'.'),((x+2,y),'.')]) c) . maybe Nothing (\g' -> placeBox g' (next cr d) d) $ placeBox g (next (x-1,y) d) d
  | otherwise = error "bad state"
    where cr = (x+1,y)
          l = findAt g c
          r = findAt g cr

moveBoxL :: Grid Char -> Coord -> Maybe (Grid Char)
moveBoxL g (x,y) = fmap (\line -> pre ++ line:post) l'
  where (pre,l:post) = splitAt y g
        x' = length (head g) - x - 1
        l' = fmap reverse $ moveBoxR' (reverse l) x'

moveBoxR :: Grid Char -> Coord -> Maybe (Grid Char)
moveBoxR g (x,y) = fmap (\l -> prevRs ++ l:restRs) line'
  where (prevRs,(line:restRs)) = splitAt y g
        line' = moveBoxR' line x

moveBoxR' :: [Char] -> Int -> Maybe [Char]
moveBoxR' cs n = case c of
                   '#' -> Nothing
                   _ -> Just $ prev ++ ('.':boxes) ++ after
  where (prev,rest) = splitAt n cs
        (boxes,(c:after)) = span (`elem` "[]") rest

placeBox' :: Grid Char -> Coord -> Grid Char
placeBox' g (x,y) = setCoords g [((x,y),'['),((x+1,y),']')]

next :: Coord -> Char -> Coord
next (x,y) c = case c of
                 '^' -> (x,y-1)
                 '>' -> (x+1,y)
                 'v' -> (x,y+1)
                 '<' -> (x-1,y)
                 _ -> error "bad direction"

parse :: String -> (State,String)
parse = parse' False

parse' :: Bool -> String -> (State,String)
parse' w s = ((setCoord coord '.' g', coord), ins)
  where (g,ins) = fmap (concat.tail) . span (not.null) . lines $ s
        g' = if w then map (concatMap wide) g else g
        coord = head . findAll g' $ '@'

wide :: Char -> [Char]
wide c = case c of
           'O' -> "[]"
           '@' -> "@."
           '.' -> ".."
           '#' -> "##"
           _ -> error "bad map char"

_drawState :: State -> IO ()
_drawState (g,c) = do
  draw (setCoord c '@' g)

