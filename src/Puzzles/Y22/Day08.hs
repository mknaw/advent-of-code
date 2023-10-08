module Puzzles.Y22.Day08
  ( day08aSolve,
    day08bSolve,
  )
where

import Data.Bifunctor (first)
import Data.Char (digitToInt)
import qualified Data.List as L
import qualified Data.Matrix as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Lib.Matrix
import Lib.Utils
import Puzzles.Puzzles

day08aSolve :: PuzzleSolve (M.Matrix Int) Int
day08aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day08a
    }

day08bSolve :: PuzzleSolve (M.Matrix Int) Int
day08bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day08b
    }

parse' :: T.Text -> M.Matrix Int
parse' = M.fromLists . map (map digitToInt) . lines . T.unpack

day08a :: M.Matrix Int -> Int
day08a !m =
  countTrue $
    L.foldl1'
      (M.elementwise (||))
      [ mapRowWise visible m,
        mapColWise visible m,
        mapRowWise (V.reverse . visible . V.reverse) m,
        mapColWise (V.reverse . visible . V.reverse) m
      ]
  where
    visible :: V.Vector Int -> V.Vector Bool
    visible v = V.cons True $ V.zipWith (>) (V.tail v) (V.scanl1' max v)

day08b :: M.Matrix Int -> Int
day08b !m = maximum $ map (visibilityScore m) coords'
  where
    f (i, j) = and [i > 1, i < M.nrows m, j > 1, j < M.ncols m]
    coords' = filter f $ coords m

visibilityScore :: M.Matrix Int -> (Int, Int) -> Int
visibilityScore m (i, j) = L.foldl1' (*) $ score (m M.! (i, j)) <$> [r1, r2, c1, c2]
  where
    (r1, r2) = first (V.tail . V.reverse) . V.splitAt j $ M.getRow i m
    (c1, c2) = first (V.tail . V.reverse) . V.splitAt i $ M.getCol j m

    score :: Int -> V.Vector Int -> Int
    score from v = V.length vis + extra
      where
        (vis, next) = V.span (< from) v
        extra = boolToInt . not . V.null $ next
