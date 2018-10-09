module Lib
    ( formatGrid
    , outputGrid
    , findWord
    , findWordInLine
    , findWords
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , cell2char
    , Cell(Cell,Indent)
    ) where

import Data.Maybe (catMaybes)
import Data.List (isInfixOf, transpose)

data Cell = Cell (Integer, Integer) Char
          | Indent
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

coordsGrid :: Grid (Integer, Integer)
coordsGrid =
  let rows = map repeat [0 ..]
      cols = repeat [0 ..]
  in zipOverGrid rows cols

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid


outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

-- findWord :: Grid -> String -> Bool
-- findWord grid word = or (map (findWordInLine word) grid)
-- we can also use this notation cause the parenthesis goes to thje end of the line
-- findWord grid word = or $ map (findWordInLine word) grid
-- map over all the lines and try to find the word. then run a logic operator over all the result Then decide whether there is a match or not
-- To look for reversed word we can use the reverse function
-- the reverse function will reverse the grid up down so we need to use map to reverse each line
-- findWord grid word =
--   let lines = grid ++ (map reverse grid)
--   in or $ map (findWordInLine word) lines
-- All of that is good and stuff but we would like a list of all the words that exist not true/false

getLines :: Grid Cell -> [[Cell]]
getLines grid =
  let lines = grid ++ (transpose grid) ++ (transpose (skew grid)) ++  (transpose (skew (reverse grid)))
  in lines ++ (map reverse lines)
-- lines = horizontal ++ vertical ++ diagonal1 + diagonal2

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let foundWords = map (findWord grid ) words
  in catMaybes foundWords

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine = isInfixOf
-- the func can also be written as findWordInLine word line = isInfixOf word line
-- but as we can see the 2 funct have the same inputs so we just can use "findWordInLine = isInfixOf"
-- we can use something = isInfix String String or a better notation would be String `isInfixOf` String
