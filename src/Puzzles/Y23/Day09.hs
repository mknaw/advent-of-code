module Puzzles.Y23.Day09
  ( day09aSolve,
    day09bSolve,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

day09aSolve :: PuzzleSolve [[Int]] Int
day09aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day09 True (+)
    }

day09bSolve :: PuzzleSolve [[Int]] Int
day09bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day09 False (flip (-))
    }

parse' :: T.Text -> [[Int]]
parse' = parseInput $ (parseInt `sepEndBy1` hspace) `sepEndBy1` newline

diff :: [Int] -> [Int]
diff (x : y : xs) = (y - x) : diff (y : xs)
diff _ = []

day09 :: Bool -> (Int -> Int -> Int) -> [[Int]] -> Int
day09 takesLast op = sum . fmap extrapolate
  where
    maybeReverse :: [a] -> [a]
    maybeReverse = if takesLast then reverse else id

    extrapolate =
      L.foldl1' op
        . fmap (head . maybeReverse)
        . reverse
        . takeWhile (not . all (== 0))
        . iterate diff
