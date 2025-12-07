module Puzzles.Y25.Day07
  ( day07aSolve,
    day07bSolve,
  )
where

import Control.Lens (Bifunctor (bimap))
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (uncons)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Puzzles.Puzzles

day07aSolve :: PuzzleSolve (IntMap Int, [IntMap Int]) Int
day07aSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = sum . fst . uncurry simulate
    }

day07bSolve :: PuzzleSolve (IntMap Int, [IntMap Int]) Int
day07bSolve =
  PuzzleSolve
    { _parse = parse,
      _solve = sum . snd . uncurry simulate
    }

parse :: T.Text -> (IntMap Int, [IntMap Int])
parse =
  -- mapifying the `levels` is mostly done for convenience,
  -- then can easily do stuff like `IM.intersection`;
  -- the values don't really convey any info otherwise (just 1s)
  -- TBH the version I had for part A with two IntSets was much better
  -- on benchmark, but whatever, nice to have a single approach.
  bimap (mapify 'S') (mapify '^' <$>)
    . fromJust
    . uncons
    . lines
    . T.unpack
  where
    idxs :: Char -> String -> [Int]
    idxs c s = do
      (i, c') <- zip [0 ..] s
      [i | c == c']

    mapify :: Char -> String -> IntMap Int
    mapify c = IM.fromAscList . (flip zip) (repeat 1) . idxs c

simulate :: IntMap Int -> [IntMap Int] -> ([Int], IntMap Int)
simulate beams levels = runState (mapM go levels) beams
  where
    go :: IntMap Int -> State (IntMap Int) Int
    go lvl = do
      bs <- get
      let hits = IM.intersection bs lvl
      modify' $ IM.unionWith (+) (split hits) . (flip IM.difference) hits
      return $ IM.size hits

    split :: IntMap Int -> IntMap Int
    split hits = IM.fromListWith (+) $ do
      (i, count) <- IM.toList hits
      [(i - 1, count), (i + 1, count)]
