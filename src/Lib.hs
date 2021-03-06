module Lib
    ( formatGrid
    , outputGrid
    , findWord
    , findWordInLine
    , findWordInCellLinePrefix
    , findWords
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , cell2char
    , Cell(Cell,Indent)
    , Game(gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , formatGame
    , completed
    , makeRandomGrid
    , fillInBlanks
    , formatGameGrid
    ) where

import Data.Maybe (catMaybes, listToMaybe)
import Data.List (isInfixOf, transpose)
import System.Random
import Data.Char (toLower)
import qualified Data.Map as M

data Game = Game {
              gameGrid :: Grid Cell,
              gameWords :: M.Map String (Maybe [Cell])
            }
            deriving Show
data Cell = Cell (Integer, Integer) Char
          | Indent
            deriving (Eq, Ord, Show)
type Grid a = [[a]]

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
    Nothing -> game
    Just cs ->
      let dict = gameWords game
          newDict = M.insert word foundWord dict
      in game { gameWords = newDict }

formatGame :: Game -> String
formatGame game = formatGameGrid game
                ++ "\n\n"
                ++ "You found "
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)
                ++ " words."

makeRandomGrid gen =
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen1
  in row : makeRandomGrid gen2

fillInBlanks gen grid =
  let r = makeRandomGrid gen
      fill '_' r = r
      fill c _ = c
  in zipOverGridWith fill grid r

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

formatGameGrid :: Game -> String
formatGameGrid game =
  let grid = gameGrid game
      dict = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell =
        let char = cell2char cell
        in if cell `elem` cellSet then char else toLower char
      charGrid = mapOverGrid formatCell grid
  in unlines charGrid

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
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
  let foundWords = map (findWord grid ) words
  in catMaybes foundWords

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    Nothing -> findWordInLine word (tail line)
    cs@(Just _) -> cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
  = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

-- the func can also be written as findWordInLine word line = isInfixOf word line
-- but as we can see the 2 funct have the same inputs so we just can use "findWordInLine = isInfixOf"
-- we can use something = isInfix String String or a better notation would be String `isInfixOf` String
