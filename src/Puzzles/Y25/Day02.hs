module Puzzles.Y25.Day02
  ( day02aSolve,
    day02bSolve,
  )
where

import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

day02aSolve :: PuzzleSolve [(Int, Int)] Int
day02aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = sum . concatMap (solveOne 2)
    }

day02bSolve :: PuzzleSolve [(Int, Int)] Int
day02bSolve =
  PuzzleSolve
    { _parse = parse',
      -- TODO converting to Set to dedup is pretty weak,
      -- gotta think of a better way here.
      _solve = sum . S.fromList . concatMap solveAll
    }

parse' :: T.Text -> [(Int, Int)]
parse' = parseInput $ parseRange `sepBy` char ','
  where
    parseRange = (,) <$> parseInt <* char '-' <*> parseInt

numDigits :: Int -> Int
numDigits 0 = 1
numDigits n = length $ takeWhile (> 0) $ iterate (`div` 10) (abs n)

initialCandidate :: Int -> Int -> Int
initialCandidate d x
  | digits `mod` d == 0 = x `div` tens
  | otherwise = 10 ^ (digits `div` d)
  where
    digits = numDigits x
    tens = 10 ^ (digits - (digits `div` d))

-- Feels like much too much calculating `numDigits`, but somehow this ends up
-- being faster than a couple versions I tried with keeping track of the multiplier
mkCandidate :: Int -> Int -> Int
mkCandidate d x = (* x) . sum $ [10 ^ (numDigits x * i) | i <- [0 .. (d - 1)]]

solveOne :: Int -> (Int, Int) -> [Int]
solveOne d (lb, ub) = takeWhile (<= ub) . dropWhile (< lb) $ xs
  where
    xs = (mkCandidate d) <$> (iterate (+ 1) . (initialCandidate d) $ lb)

solveAll :: (Int, Int) -> [Int]
solveAll (lb, ub) = do
  d <- [2 .. numDigits ub]
  solveOne d (lb, ub)
