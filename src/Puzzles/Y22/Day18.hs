module Puzzles.Y22.Day18
  ( day18aSolve,
    day18bSolve,
  )
where

import Control.Lens.Getter ((^.))
import qualified Data.Set as S
import qualified Data.Text as T
import Lib.Parse
import Linear.V3
import Puzzles.Puzzles
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char

type Droplet = S.Set (V3 Int)

day18aSolve :: PuzzleSolve Droplet Int
day18aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day18a
    }

day18bSolve :: PuzzleSolve Droplet Int
day18bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day18b
    }

parse' :: T.Text -> Droplet
parse' = parseInput . fmap S.fromList $ parseV3 `sepEndBy` newline

(<->) :: V3 Int -> V3 Int -> Int
V3 x1 y1 z1 <-> V3 x2 y2 z2 = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

sideCount :: Droplet -> V3 Int -> Int
sideCount droplet v3 = (6 -) . length . S.filter (\v3' -> v3 <-> v3' == 1) $ droplet

day18a :: Droplet -> Int
day18a droplet = sum . map (sideCount droplet) . S.toList $ droplet

_maxX :: Droplet -> Int
_maxX = maximum . S.map (^. _x)

_maxY :: Droplet -> Int
_maxY = maximum . S.map (^. _y)

_maxZ :: Droplet -> Int
_maxZ = maximum . S.map (^. _z)

inDroplet :: Droplet -> V3 Int -> Bool
inDroplet droplet v3 = v3 `elem` droplet

inBounds :: Droplet -> V3 Int -> Bool
inBounds droplet (V3 x y z) =
  and
    [ x >= 0,
      x <= _maxX droplet,
      y >= 0,
      y <= _maxY droplet,
      z >= 0,
      z <= _maxZ droplet
    ]

neighbors :: Droplet -> V3 Int -> [V3 Int]
neighbors droplet (V3 x y z) =
  filter (not . inDroplet droplet)
    . filter (inBounds droplet)
    $ [ V3 (x + 1) y z,
        V3 (x - 1) y z,
        V3 x (y + 1) z,
        V3 x (y - 1) z,
        V3 x y (z + 1),
        V3 x y (z - 1)
      ]

dfs' :: Droplet -> [V3 Int] -> Droplet -> S.Set (V3 Int)
dfs' seen [] _ = seen
dfs' seen (v3 : frontier) droplet
  | v3 `S.member` seen = dfs' seen frontier droplet
  | otherwise = dfs' (S.insert v3 seen) (neighbors droplet v3 ++ frontier) droplet

range :: Droplet -> Droplet
range droplet = S.fromList [V3 x y z | x <- [0 .. _maxX droplet], y <- [0 .. _maxY droplet], z <- [0 .. _maxZ droplet]]

isInterior :: Droplet -> Droplet -> Bool
isInterior droplet region =
  _maxX region > 0
    && _maxY region > 0
    && _maxZ region > 0
    && _maxX region < _maxX droplet
    && _maxY region < _maxY droplet
    && _maxZ region < _maxZ droplet

explore :: Droplet -> Droplet
explore droplet = go (S.toList $ range droplet) droplet
  where
    go :: [V3 Int] -> Droplet -> Droplet
    go [] d = d
    go (curr : rest) d
      | inDroplet d curr = go rest d
      | otherwise = go rest' d'
      where
        explored = dfs' (S.singleton curr) (neighbors d curr) d
        rest' = filter (not . (`S.member` explored)) rest
        d' =
          if isInterior d explored
            then S.union d explored
            else d

day18b :: Droplet -> Int
day18b = day18a . explore
