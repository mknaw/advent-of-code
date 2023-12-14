{-# LANGUAGE RankNTypes #-}

module Puzzles.Y23.Day13
  ( day13aSolve,
    day13bSolve,
  )
where

import Control.Lens
import qualified Data.List.Safe as Safe
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tuple.Extra ((&&&))
import Lib.Utils (indicesWhere, symmetricDifference)
import Lib.Utils.Geometry
import Linear.V2
import Puzzles.Puzzles

day13aSolve :: PuzzleSolve [(Point, S.Set Point)] Int
day13aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day13 0
    }

day13bSolve :: PuzzleSolve [(Point, S.Set Point)] Int
day13bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day13 1
    }

parse' :: T.Text -> [(Point, S.Set Point)]
parse' = fmap (gridSizeOf &&& parseGridToSet (== '#')) . splitOn "\n\n" . T.unpack

nonSymmetricCount :: Lens' (V2 Int) Int -> Int -> S.Set Point -> Int -> Int
nonSymmetricCount lns m s x = S.size $ symmetricDifference l' r
  where
    (l, r) = S.partition ((< x) . (^. lns)) . S.filter ((< 2 * x) . (^. lns)) $ s
    l' = S.filter ((< m) . (^. lns)) . S.map (over lns (subtract 1 . (+ 2 * x) . negate)) $ l

findAxisOfSymmetry :: Int -> Lens' (V2 Int) Int -> Int -> S.Set Point -> Maybe Int
findAxisOfSymmetry d lns m p =
  fmap (+ 1)
    . Safe.head
    . indicesWhere ((== d) . nonSymmetricCount lns m p)
    $ [1 .. m - 1]

day13 :: Int -> [(Point, S.Set Point)] -> Int
day13 d = sum . fmap (uncurry f)
  where
    f :: Point -> S.Set Point -> Int
    f (V2 x y) p = if null xs then error . show $ p else head xs
      where
        xs = catMaybes $ [findAxisOfSymmetry d _x x, fmap (* 100) . findAxisOfSymmetry d _y y] ?? p
