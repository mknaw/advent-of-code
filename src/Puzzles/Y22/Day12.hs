module Puzzles.Y22.Day12
  ( day12aSolve,
    day12bSolve,
  )
where

import Data.Char (ord)
import qualified Data.List as L
import qualified Data.Matrix as M
import qualified Data.Maybe as M
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Matrix
import Puzzles.Puzzles

type HtMap = M.Matrix Char

day12aSolve :: PuzzleSolve HtMap Int
day12aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day12a
    }

day12bSolve :: PuzzleSolve HtMap Int
day12bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day12b
    }

data Path = Path
  { _dist :: Int,
    _curr :: Coord,
    _char :: Char,
    _visited :: S.Set Coord
  }
  deriving (Eq, Show)

instance Ord Path where
  compare p1 p2 =
    case compare (_dist p1) (_dist p2) of
      EQ -> compare (_char p2) (_char p1)
      x -> x

parse' :: T.Text -> HtMap
parse' = M.fromLists . map T.unpack . T.lines

findLandmark :: Char -> HtMap -> Coord
findLandmark c m = (1 + k `div` M.ncols m, 1 + k `rem` M.ncols m)
  where
    k = fst . head . filter ((==) c . snd) . zip [0 ..] . M.toList $ m

type ViabilityTest = (Char -> Char -> Bool)

isViableChar :: ViabilityTest
isViableChar 'S' to = isViableChar 'a' to
isViableChar from 'E' = isViableChar from 'z'
isViableChar from to = ord to - ord from < 2

getFrontier :: HtMap -> ViabilityTest -> Coord -> S.Set Coord
getFrontier m isV curr =
  S.fromList
    . filter (\c -> isV (m M.! curr) (m M.! c))
    $ neighbors m curr

bfs' ::
  HtMap ->
  -- | target
  Char ->
  -- | viability fn
  ViabilityTest ->
  -- | frontier
  S.Set Coord ->
  -- | visited
  S.Set Coord ->
  -- | distance
  Int ->
  Maybe Int
bfs' m target isV frontier visited dist
  | S.null frontier = Nothing
  | target `S.member` S.map (m M.!) frontier = Just dist
  | otherwise = bfs' m target isV frontier' visited' (dist + 1)
  where
    frontier' = flip S.difference visited $ L.foldl' S.union S.empty (S.map (getFrontier m isV) frontier)
    visited' = S.union visited frontier

bfs :: HtMap -> Char -> ViabilityTest -> Coord -> Maybe Int
bfs m target isV start = bfs' m target isV (S.singleton start) S.empty 0

day12a :: HtMap -> Int
day12a m = M.fromJust . bfs m 'E' isViableChar . findLandmark 'S' $ m

day12b :: HtMap -> Int
day12b m = M.fromJust . bfs m 'a' (flip isViableChar) . findLandmark 'E' $ m
