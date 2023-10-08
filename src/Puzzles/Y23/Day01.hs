module Puzzles.Y23.Day01
  ( day01aSolve,
    day01bSolve,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day01aSolve :: PuzzleSolve [[Int]] Int
day01aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day01a
    }

day01bSolve :: PuzzleSolve [[Int]] Int
day01bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day01b
    }

parse' :: T.Text -> [[Int]]
parse' = parseInput $ parseIntLines `sepEndBy` newline

day01a :: [[Int]] -> Int
day01a = maximum . map sum

day01b :: [[Int]] -> Int
day01b = sum . take 3 . reverse . L.sort . map sum
