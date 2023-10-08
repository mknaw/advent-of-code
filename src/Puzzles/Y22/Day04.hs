module Puzzles.Y22.Day04
  ( day04aSolve,
    day04bSolve,
  )
where

import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

type Range = (Int, Int)

day04aSolve :: PuzzleSolve [(Range, Range)] Int
day04aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day04a
    }

day04bSolve :: PuzzleSolve [(Range, Range)] Int
day04bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day04b
    }

parse' :: T.Text -> [(Range, Range)]
parse' = parseInput $ parseRangePair `sepEndBy` newline
  where
    parseRange :: Parser Range
    parseRange = do
      a <- parseInt
      char '-'
      b <- parseInt
      return (a, b)

    parseRangePair :: Parser (Range, Range)
    parseRangePair = do
      a <- parseRange
      char ','
      b <- parseRange
      return (a, b)

day04a :: [(Range, Range)] -> Int
day04a = length . filter f . map lbSort
  where
    f ((a, b), (c, d))
      | a == c = True
      | otherwise = b >= d

day04b :: [(Range, Range)] -> Int
day04b = length . filter f . map lbSort
  where
    f ((a, b), (c, _))
      | a == c = True
      | otherwise = b >= c

lbSort :: (Range, Range) -> (Range, Range)
lbSort ((a, b), (c, d))
  | a < c = ((a, b), (c, d))
  | otherwise = ((c, d), (a, b))
