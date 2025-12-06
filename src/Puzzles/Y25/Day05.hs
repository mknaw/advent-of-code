module Puzzles.Y25.Day05
  ( day05aSolve,
    day05bSolve,
  )
where

import Data.ExtendedReal (Extended (Finite))
import Data.Interval ((<=..<=))
import qualified Data.Interval as II
import qualified Data.IntervalSet as IS
import qualified Data.Text as T
import Distribution.Compat.CharParsing (sepEndBy)
import Lib.Parse
import Lib.Utils (countTrue)
import Puzzles.Puzzles
import Text.Megaparsec.Char (char', newline)

-- Relying so much on the already implemented library is a bit cheap instead
-- of doing your own interval coding, but... take the easy wins for now.

day05aSolve :: PuzzleSolve (IS.IntervalSet Int, [Int]) Int
day05aSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = uncurry $ (countTrue id .) . fmap . flip IS.member
    }

day05bSolve :: PuzzleSolve (IS.IntervalSet Int) Int
day05bSolve =
  PuzzleSolve
    { _parse = fst . parse,
      -- `II.width` is just (upper - lower), so add 1 to count inclusive bounds.
      _solve = sum . fmap ((+ 1) . II.width) . IS.toList
    }

parse :: T.Text -> (IS.IntervalSet Int, [Int])
parse = parseInput $ do
  ranges <- IS.fromList <$> parseRange `sepEndBy` newline
  newline
  ids <- parseIntLines
  return (ranges, ids)
  where
    parseRange :: Parser (II.Interval Int)
    parseRange =
      (<=..<=)
        <$> (Finite <$> parseInt)
        <*> (Finite <$> (char' '-' *> parseInt))
