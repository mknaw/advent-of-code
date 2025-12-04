module Puzzles.Y25.Day03
  ( day03aSolve,
    day03bSolve,
  )
where

import Data.Vector ((!))
import qualified Data.Vector as V
import Lib.Parse
import Puzzles.Puzzles

day03aSolve :: PuzzleSolve [V.Vector Int] Int
day03aSolve =
  PuzzleSolve
    { _parse = fmap V.fromList . parseInput parseDigitLines,
      _solve = sum . fmap (solve 2)
    }

day03bSolve :: PuzzleSolve [V.Vector Int] Int
day03bSolve =
  PuzzleSolve
    { _parse = fmap V.fromList . parseInput parseDigitLines,
      _solve = sum . fmap (solve 12)
    }

solve :: Int -> V.Vector Int -> Int
solve d input = go (-1) 0 d
  where
    go :: Int -> Int -> Int -> Int
    go p acc left
      | left == 0 = acc
      | otherwise = go i (10 * acc + input ! i) (left - 1)
      where
        ub = V.length input - left - p
        i = (+ (p + 1)) . V.maxIndex . V.slice (p + 1) ub $ input
