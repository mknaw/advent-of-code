module Puzzles.Y25.Day01
  ( day01aSolve,
    day01bSolve,
  )
where

import Control.Arrow ((&&&))
import qualified Data.Text as T
import Data.Traversable (mapAccumL)
import Puzzles.Puzzles

day01aSolve :: PuzzleSolve [Rot] Int
day01aSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = length . filter (== 0) . scanl applyRot 50
    }

day01bSolve :: PuzzleSolve [Rot] Int
day01bSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = solveB
    }

data Rot = L Int | R Int deriving (Show)

parse :: T.Text -> [Rot]
parse = fmap p . T.lines
  where
    p s =
      case T.unpack s of
        'L' : xs -> L (read xs)
        'R' : xs -> R (read xs)
        _ -> error "invalid input"

applyRot :: Int -> Rot -> Int
applyRot x (L c) = (x - c) `mod` 100
applyRot x (R c) = (x + c) `mod` 100

countClicks :: Int -> Rot -> Int
countClicks x (L c)
  | c == x = 1
  | c < x = 0
  | otherwise = (if x > 0 then 1 else 0) + (c - x) `div` 100
countClicks x (R c) = (x + c) `div` 100

solveB :: [Rot] -> Int
solveB = sum . snd . mapAccumL go 50
  where
    go :: Int -> Rot -> (Int, Int)
    go x = applyRot x &&& countClicks x
