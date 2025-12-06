module Puzzles.Y25.Day06
  ( day06aSolve,
    day06bSolve,
  )
where

import Control.Arrow ((&&&))
import Data.Char (isSpace)
import Data.List (foldl1', transpose)
import Data.List.Split (splitWhen)
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils (dropSpaces)
import Puzzles.Puzzles
import Text.Megaparsec (oneOf, sepBy, sepEndBy)
import Text.Megaparsec.Char (hspace, newline)

day06aSolve :: PuzzleSolve [(Char, [Int])] Int
day06aSolve =
  PuzzleSolve
    { _parse = parseA,
      _solve = solve
    }

day06bSolve :: PuzzleSolve (String, [String]) Int
day06bSolve =
  PuzzleSolve
    { _parse = parseB,
      _solve = uncurry solveB
    }

parseA :: T.Text -> [(Char, [Int])]
parseA = parseInput $ do
  numberRows <- transpose <$> (hspace *> parseInt `sepEndBy` hspace) `sepBy` newline
  ops <- (hspace *> (oneOf ['+', '*']) `sepEndBy` hspace)
  return $ zip ops numberRows

solve :: [(Char, [Int])] -> Int
solve = sum . fmap (uncurry (foldl1' . apply))
  where
    apply :: Char -> Int -> Int -> Int
    apply '+' = (+)
    apply '*' = (*)
    apply _ = error "invalid op"

parseB :: T.Text -> (String, [String])
parseB = ((dropSpaces . last) &&& init) . lines . T.unpack

solveB :: String -> [String] -> Int
solveB ops =
  solve
    . zip ops
    . (fmap . fmap) (read . dropSpaces)
    . splitWhen (all isSpace)
    . transpose
