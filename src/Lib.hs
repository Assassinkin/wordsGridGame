module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWord
    , findWordInLine
    , findWords
    ) where

import Data.Maybe (catMaybes)
import Data.List (isInfixOf, transpose)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

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

getLines :: Grid -> [String]
getLines grid =
  let lines = grid ++ (transpose grid) ++ (transpose (skew grid)) ++  (transpose (skew (reverse grid)))
  in lines ++ (map reverse lines)
-- lines = horizontal ++ vertical ++ diagonal1 + diagonal2

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = '_' : line

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words =
  let foundWords = map (findWord grid ) words
  in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf
-- the func can also be written as findWordInLine word line = isInfixOf word line
-- but as we can see the 2 funct have the same inputs so we just can use "findWordInLine = isInfixOf"
-- we can use something = isInfix String String or a better notation would be String `isInfixOf` String

grid = [ "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

languages = [ "BASIC"
            , "COBAL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]
