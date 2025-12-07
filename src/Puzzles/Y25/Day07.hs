module Puzzles.Y25.Day07
  ( day07aSolve,
    day07bSolve,
  )
where

import Control.Lens (Bifunctor (bimap))
import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (uncons)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Puzzles.Puzzles

day07aSolve :: PuzzleSolve (IntSet, [IntSet]) Int
day07aSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = uncurry solve
    }

day07bSolve :: PuzzleSolve (IntSet, [IntSet]) Int
day07bSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = const 0
    }

parse :: T.Text -> (IntSet, [IntSet])
parse =
  bimap (idxs 'S') (idxs '^' <$>)
    . fromJust
    . uncons
    . lines
    . T.unpack
  where
    idxs :: Char -> String -> IntSet
    idxs c s = IS.fromAscList $ do
      (i, c') <- zip [0 ..] s
      [i | c == c']

split :: IntSet -> IntSet
split hits = IS.fromList $ do
  h <- IS.toList hits
  [h - 1, h + 1]

go :: IntSet -> State IntSet Int
go level = do
  beams <- get
  let hits = IS.intersection level beams
  modify' $ IS.union (split hits) . (flip IS.difference) hits
  return $ IS.size hits

solve :: IntSet -> [IntSet] -> Int
solve beams levels = sum $ evalState (mapM go levels) beams
