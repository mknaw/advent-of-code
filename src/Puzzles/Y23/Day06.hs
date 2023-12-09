module Puzzles.Y23.Day06
  ( day06aSolve,
    day06bSolve,
  )
where

import qualified Data.Text as T
import Lib.Parse
import Lib.Utils (both)
import Puzzles.Puzzles
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char

day06aSolve :: PuzzleSolve [(Int, Int)] Int
day06aSolve =
  PuzzleSolve
    { _parse = uncurry zip . both (fmap read) . parse',
      _solve = day06
    }

day06bSolve :: PuzzleSolve [(Int, Int)] Int
day06bSolve =
  PuzzleSolve
    { _parse = (: []) . both (read . concat) . parse',
      _solve = day06
    }

parse' :: T.Text -> ([String], [String])
parse' = parseInput $ do
  string "Time:"
  space
  rawTimes <- some digitChar `sepEndBy1` space
  string "Distance:"
  space
  rawDistances <- some digitChar `sepEndBy1` space
  return (rawTimes, rawDistances)

distance :: Int -> Int -> Int
distance time hold = (time - hold) * hold

winningCount :: (Int, Int) -> Int
winningCount (time, best) = length . filter (> best) $ runs
  where
    runs = distance time <$> [1 .. time]

day06 :: [(Int, Int)] -> Int
day06 = product . fmap winningCount
